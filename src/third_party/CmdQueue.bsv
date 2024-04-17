import ClientServer :: *;
import GetPut :: *;
import FIFOF :: *;
import Vector :: *;

import DataTypes :: *;
import UserLogicSettings :: *;
import UserLogicUtils :: *;
import UserLogicTypes :: *;
import MetaData :: *;
import PrimUtils :: *;
import Ringbuf :: *;
import Ports :: *;
import Headers :: *;

interface CommandQueueController;
    interface Server#(RingbufRawDescriptor, RingbufRawDescriptor)   ringbufSrv;
    interface Client#(RingbufRawDescriptor, Bool)                   mrAndPgtManagerClt;
    interface Client#(WriteReqCommonQPC, Bool)                      qpcModifyClt;
    interface Get#(UdpConfig)                                       setNetworkParamReqOut;
    interface Get#(RawPacketReceiveMeta)                            setRawPacketReceiveMetaReqOut;
    interface Get#(Tuple2#(IndexQP, PSN))                           setRqExpectedPsnReqOut;
endinterface


(* synthesize *)
module mkCommandQueueController(CommandQueueController ifc);

    // If we need to wait for response for some cycle to finish, then we need to set this to False;
    Reg#(Bool) isDispatchingReqReg                              <- mkReg(True);
    
    FIFOF#(RingbufRawDescriptor) mrAndPgtReqQ                   <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) mrAndPgtInflightReqQ           <- mkFIFOF;
    FIFOF#(Bool) mrAndPgtRespQ                                  <- mkFIFOF;

    FIFOF#(WriteReqCommonQPC) qpcReqQ                           <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) qpcInflightReqQ                <- mkFIFOF;
    FIFOF#(Bool) qpcRespQ                                       <- mkFIFOF;

    FIFOF#(UdpConfig)            setNetworkParamReqQ            <- mkFIFOF;
    FIFOF#(RawPacketReceiveMeta) setRawPacketReceiveMetaReqQ    <- mkFIFOF;
    FIFOF#(Tuple2#(IndexQP, PSN))    setRqExpectedPsnReqQ       <- mkFIFOF;


    RingbufDescriptorReadProxy#(COMMAND_QUEUE_DESCRIPTOR_MAX_IN_USE_SEG_COUNT) descReadProxy <- mkRingbufDescriptorReadProxy;
    RingbufDescriptorWriteProxy#(COMMAND_QUEUE_DESCRIPTOR_MAX_IN_USE_SEG_COUNT) descWriteProxy <- mkRingbufDescriptorWriteProxy;
    
    rule dispatchRingbufRequestDescriptors if (descWriteProxy.canSetDesc && isDispatchingReqReg);
        Vector#(COMMAND_QUEUE_DESCRIPTOR_MAX_IN_USE_SEG_COUNT, RingbufRawDescriptor) respRawDescSeg = ?;

        let {reqSegBuf, headDescIdx} <- descReadProxy.getWideDesc;
        RingbufRawDescriptor rawDesc = reqSegBuf[headDescIdx];
        let opcode = getCmdQueueOpcodeFromRawRingbufDescriptor(rawDesc);
        case (unpack(truncate(opcode)))
            CmdQueueOpcodeUpdateMrTable, CmdQueueOpcodeUpdatePGT: begin
                mrAndPgtReqQ.enq(rawDesc);
                mrAndPgtInflightReqQ.enq(rawDesc); // TODO, we can simplify this to only include 32-bit user_data field
                isDispatchingReqReg <= False;
            end
            CmdQueueOpcodeQpManagement: begin
                CmdQueueReqDescQpManagementSeg0 desc0 = unpack(reqSegBuf[0]);
                
                let ent = EntryCommonQPC {
                    // isError:        desc0.isError,
                    peerQPN   :     desc0.peerQPN,
                    qpnKeyPart:     getKeyQP(desc0.qpn), 
                    pdHandler:      desc0.pdHandler,
                    qpType:         desc0.qpType,
                    rqAccessFlags:  desc0.rqAccessFlags,
                    pmtu:           desc0.pmtu
                };

                qpcInflightReqQ.enq(rawDesc);
                qpcReqQ.enq(
                    WriteReqCommonQPC {
                        qpn: desc0.qpn,
                        ent: desc0.isValid ? tagged Valid ent : tagged Invalid
                    }
                );
                setRqExpectedPsnReqQ.enq(tuple2(getIndexQP(desc0.qpn), 0));
                isDispatchingReqReg <= False;
                $display("time=%0t: ", $time, "SOFTWARE DEBUG POINT ", "Hardware receive cmd queue descriptor: ", fshow(desc0));
            end
            CmdQueueOpcodeSetNetworkParam: begin
                CmdQueueReqDescSetNetworkParam reqDesc = unpack(rawDesc);
                let udpConfig = UdpConfig{
                        macAddr :   reqDesc.macAddr,
                        ipAddr  :   reqDesc.ipAddr,
                        netMask :   reqDesc.netMask,
                        gateWay :   reqDesc.gateWay
                };
                setNetworkParamReqQ.enq(udpConfig);
                CmdQueueRespDescOnlyCommonHeader respDesc = unpack(pack(reqDesc));
                respDesc.commonHeader.isSuccessOrNeedSignalCplt = True;
                respDesc.commonHeader.valid = True;
                respDesc.commonHeader.extraSegmentCnt = 0;
                respRawDescSeg[0] = pack(respDesc);
                descWriteProxy.setWideDesc(respRawDescSeg, 0);
                $display("time=%0t: ", $time, "SOFTWARE DEBUG POINT ", "Hardware receive cmd queue descriptor: ", fshow(reqDesc));
                $display("time=%0t: ", $time, "SOFTWARE DEBUG POINT ", "Hardware Send cmd queue response: ", fshow(respDesc));
            end
            CmdQueueOpcodeSetRawPacketReceiveMeta: begin
                CmdQueueReqDescSetRawPacketReceiveMeta reqDesc = unpack(rawDesc);
                setRawPacketReceiveMetaReqQ.enq(RawPacketReceiveMeta{
                    writeBaseAddr: reqDesc.writeBaseAddr,
                    writeMrKey   : reqDesc.writeMrKey
                });

                CmdQueueRespDescOnlyCommonHeader respDesc = unpack(pack(reqDesc));
                respDesc.commonHeader.isSuccessOrNeedSignalCplt = True;
                respDesc.commonHeader.valid = True;
                respDesc.commonHeader.extraSegmentCnt = 0;
                respRawDescSeg[0] = pack(respDesc);
                descWriteProxy.setWideDesc(respRawDescSeg, 0);
                $display("time=%0t: ", $time, "SOFTWARE DEBUG POINT ", "Hardware receive cmd queue descriptor: ", fshow(reqDesc));
                $display("time=%0t: ", $time, "SOFTWARE DEBUG POINT ", "Hardware Send cmd queue response: ", fshow(respDesc));
            end
        endcase

    endrule

    rule gatherResponse if (descWriteProxy.canSetDesc && !isDispatchingReqReg);
        // TODO should we use a fair algorithm here?
        
        Vector#(COMMAND_QUEUE_DESCRIPTOR_MAX_IN_USE_SEG_COUNT, RingbufRawDescriptor) respRawDescSeg = ?;
        

        if (mrAndPgtRespQ.notEmpty) begin
            // Note: since the update MR and update PGT req both need only one descriptor and the response only care for
            // `isSuccessOrNeedSignalCplt` field, so we use CmdQueueRespDescUpdatePGT to handle both resp.
            CmdQueueRespDescUpdatePGT respDesc = unpack(mrAndPgtInflightReqQ.first);
            respDesc.commonHeader.isSuccessOrNeedSignalCplt = mrAndPgtRespQ.first;
            respDesc.commonHeader.valid = True;
            respDesc.commonHeader.extraSegmentCnt = 0;
            mrAndPgtInflightReqQ.deq;
            mrAndPgtRespQ.deq;
            respRawDescSeg[0] = pack(respDesc);
            descWriteProxy.setWideDesc(respRawDescSeg, 0);
            isDispatchingReqReg <= True;
            $display("time=%0t: ", $time, "SOFTWARE DEBUG POINT ", "Hardware Send cmd queue response: ", fshow(respDesc));
        end 
        else if (qpcRespQ.notEmpty) begin 
            qpcRespQ.deq;
            qpcInflightReqQ.deq;
           
            CmdQueueRespDescQpManagementSeg0 respDesc = unpack(qpcInflightReqQ.first);
            respDesc.commonHeader.isSuccessOrNeedSignalCplt = qpcRespQ.first;
            respDesc.commonHeader.valid = True;
            respDesc.commonHeader.extraSegmentCnt = 0;
            respRawDescSeg[0] = pack(respDesc);
            descWriteProxy.setWideDesc(respRawDescSeg, 0);
            isDispatchingReqReg <= True;
            $display("time=%0t: ", $time, "SOFTWARE DEBUG POINT ", "Hardware Send cmd queue response: ", fshow(respDesc));
        end
    endrule

    interface ringbufSrv = toGPServer(descReadProxy.ringbufConnector, descWriteProxy.ringbufConnector);
    interface mrAndPgtManagerClt = toGPClient(mrAndPgtReqQ, mrAndPgtRespQ);
    interface qpcModifyClt = toGPClient(qpcReqQ, qpcRespQ);

    interface setNetworkParamReqOut = toGet(setNetworkParamReqQ);
    interface setRawPacketReceiveMetaReqOut = toGet(setRawPacketReceiveMetaReqQ);
    interface setRqExpectedPsnReqOut = toGet(setRqExpectedPsnReqQ);
endmodule