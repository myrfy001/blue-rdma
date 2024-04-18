import ClientServer :: * ;
import GetPut :: *;
import Connectable :: *;
import PAClib :: *;
import Gearbox :: *;
import Vector :: *;
import Ports :: *;

import DataTypes :: *;
import RdmaUtils :: *;



import UserLogicSettings :: *;
import UserLogicTypes :: *;


function RingbufRawDescriptorOpcode getCmdQueueOpcodeFromRawRingbufDescriptor(RingbufRawDescriptor desc);
    return pack(desc)[valueOf(CMD_QUEUE_RINGBUF_DESC_OPCODE_OFFSET) + valueOf(CMD_QUEUE_RINGBUF_DESC_OPCODE_LENGTH) - 1 :valueOf(CMD_QUEUE_RINGBUF_DESC_OPCODE_OFFSET)];
endfunction

module mkFakeClient(Client#(t_req, t_resp));
    Reg#(Bool) t <- mkReg(False);
    interface Get request;
        method ActionValue#(t_req) get() if (t);
            return ?;
        endmethod
    endinterface
    interface Put response;
        method Action put(t_resp val) if (t);
        endmethod
    endinterface
endmodule



interface UdpGearbox;
    interface Put#(DataTypes::DataStream) txIn;
    interface Get#(Ports::DataStream) txOut;

    interface Put#(Ports::DataStream) rxIn;
    interface PipeOut#(DataTypes::DataStreamEn) rxOut;

    interface Put#(Ports::DataStream) rxRawIn;
    interface PipeOut#(DataTypes::DataStreamEn) rxRawOut;
endinterface

typedef 2 UDP_GEARBOX_WIDE_VECTOR_LEN;
typedef 1 UDP_GEARBOX_NARROW_VECTOR_LEN;

module mkUdpGearbox#(Clock rdmaClock, Reset rdmaReset, Clock udpClock, Reset udpReset)(UdpGearbox);

    Gearbox#(UDP_GEARBOX_WIDE_VECTOR_LEN, UDP_GEARBOX_NARROW_VECTOR_LEN, Ports::DataStream) txGearbox <- mkNto1Gearbox(rdmaClock, rdmaReset, udpClock, udpReset);
    Gearbox#(UDP_GEARBOX_NARROW_VECTOR_LEN, UDP_GEARBOX_WIDE_VECTOR_LEN, Ports::DataStream) rxGearbox <- mk1toNGearbox(udpClock, udpReset, rdmaClock, rdmaReset);
    Gearbox#(UDP_GEARBOX_NARROW_VECTOR_LEN, UDP_GEARBOX_WIDE_VECTOR_LEN, Ports::DataStream) rxRawGearbox <- mk1toNGearbox(udpClock, udpReset, rdmaClock, rdmaReset);


    Reg#(Bool) isRxRdmaStreamOddBeatReg <- mkReg(True, clocked_by udpClock, reset_by udpReset);
    Reg#(Bool) isRxRawStreamOddBeatReg <- mkReg(True, clocked_by udpClock, reset_by udpReset);
    Reg#(Bool) isRxRdmaStreamInsertExtraFakeBeatReg <- mkReg(False, clocked_by udpClock, reset_by udpReset);
    Reg#(Bool) isRxRawStreamInsertExtraFakeBeatReg <- mkReg(False, clocked_by udpClock, reset_by udpReset);

    function DataTypes::DataStreamEn narrow2Normal(Vector#(2, Ports::DataStream) inVec);
        let ret = DataTypes::DataStreamEn {
            data: {inVec[1].data, inVec[0].data},
            byteEn: {inVec[1].byteEn, inVec[0].byteEn},
            isFirst: inVec[0].isFirst,
            isLast: inVec[1].isLast
        };
        return ret;
    endfunction

    rule handleRxRdmaStreamInsertExtraFakeBeat if (isRxRdmaStreamInsertExtraFakeBeatReg);
        Vector#(UDP_GEARBOX_NARROW_VECTOR_LEN, Ports::DataStream) vec = newVector;
        vec[0] = Ports::DataStream{
            data: unpack(0),
            byteEn: unpack(0),
            isFirst: False,
            isLast: True
        };
        rxGearbox.enq(vec);
        isRxRdmaStreamInsertExtraFakeBeatReg <= False;
    endrule

    rule handleRxRawStreamInsertExtraFakeBeat if (isRxRawStreamInsertExtraFakeBeatReg);
        Vector#(UDP_GEARBOX_NARROW_VECTOR_LEN, Ports::DataStream) vec = newVector;
        vec[0] = Ports::DataStream{
            data: unpack(0),
            byteEn: unpack(0),
            isFirst: False,
            isLast: True
        };
        rxRawGearbox.enq(vec);
        isRxRawStreamInsertExtraFakeBeatReg <= False;
    endrule


    interface Put txIn;
        method Action put(DataTypes::DataStream ds);
            Vector#(UDP_GEARBOX_WIDE_VECTOR_LEN, Ports::DataStream) vec = newVector;

            let dsEn = dataStream2DataStreamEn(ds);

            vec[0].data = swapEndian(truncateLSB(dsEn.data));
            vec[1].data = swapEndian(truncate(dsEn.data));

            let onlyHasHalfBeat = ds.byteNum <= fromInteger(valueOf(DATA_BUS_NARROW_BYTE_WIDTH));
            if (onlyHasHalfBeat) begin
                vec[0].byteEn = swapEndianBit(truncateLSB(dsEn.byteEn));
                vec[1].byteEn = 0;
            end
            else begin
                vec[0].byteEn = -1;
                vec[1].byteEn = swapEndianBit(truncate(dsEn.byteEn));
            end
 
            vec[0].isFirst = ds.isFirst;
            vec[1].isFirst = False;
            vec[0].isLast = ds.isLast && onlyHasHalfBeat;
            vec[1].isLast = ds.isLast && !onlyHasHalfBeat;
    
            txGearbox.enq(vec);
        endmethod
    endinterface

    interface Get txOut;
        method ActionValue#(Ports::DataStream) get;
            txGearbox.deq;
            $display("time=%0t: ", $time,"rdma put data to udp = ", fshow(txGearbox.first[0]));
            return txGearbox.first[0];
        endmethod
    endinterface

    interface Put rxIn;
        method Action put(Ports::DataStream ds) if (!isRxRdmaStreamInsertExtraFakeBeatReg);
            if (isRxRdmaStreamOddBeatReg && ds.isLast) begin
                isRxRdmaStreamInsertExtraFakeBeatReg <= True;
            end
            else begin
                isRxRdmaStreamOddBeatReg <= !isRxRdmaStreamOddBeatReg;
            end
            Vector#(UDP_GEARBOX_NARROW_VECTOR_LEN, Ports::DataStream) vec = newVector;
            vec[0] = ds;
            rxGearbox.enq(vec);
        endmethod
    endinterface

    interface PipeOut rxOut;
        method Action deq = rxGearbox.deq;
        method first = narrow2Normal(rxGearbox.first);
        method notEmpty = rxGearbox.notEmpty;
    endinterface

    interface Put rxRawIn;
        method Action put(Ports::DataStream ds) if (!isRxRawStreamInsertExtraFakeBeatReg);
            if (isRxRawStreamOddBeatReg && ds.isLast) begin
                isRxRawStreamInsertExtraFakeBeatReg <= True;
            end
            else begin
            isRxRawStreamOddBeatReg <= !isRxRawStreamOddBeatReg;
            end

            Vector#(UDP_GEARBOX_NARROW_VECTOR_LEN, Ports::DataStream) vec = newVector;
            vec[0] = ds;
            rxRawGearbox.enq(vec);
        endmethod
    endinterface

    interface PipeOut rxRawOut;
        method Action deq = rxRawGearbox.deq;
        method first = narrow2Normal(rxRawGearbox.first);
        method notEmpty = rxRawGearbox.notEmpty;
    endinterface
       
endmodule