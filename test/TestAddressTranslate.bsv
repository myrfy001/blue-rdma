import ClientServer :: *;
import Cntrs :: *;
import FIFOF :: *;
import GetPut :: *;
import PAClib :: *;
import Vector :: *;

import DataTypes :: *;
import Headers :: *;
import MetaData :: *;
import PrimUtils :: *;
import Settings :: *;
import Utils :: *;
import Utils4Test :: *;

import AddressTranslate :: *;

(* doc = "testcase" *)
module mkTestBramCache(Empty);
    let dut <- mkBramCache;

    // Use address counter to avoid write address conflict
    Count#(BramCacheAddr)       bramCacheAddrReg <- mkCount(0);
    PipeOut#(BramCacheData) bramCacheDataPipeOut <- mkGenericRandomPipeOut;
    // PipeOut#(BramCacheAddr) bramCacheAddrPipeOut <- mkGenericRandomPipeOut;

    FIFOF#(BramCacheAddr) bramCacheAddrQ <- mkFIFOF;
    FIFOF#(BramCacheData) bramCacheDataQ <- mkFIFOF;

    let countDown <- mkCountDown(valueOf(MAX_CMP_CNT));

    rule writeBramCache;
        // let bramCacheAddr = bramCacheAddrPipeOut.first;
        // bramCacheAddrPipeOut.deq;
        let bramCacheAddr = bramCacheAddrReg;
        bramCacheAddrReg.incr(1);
        let bramCacheData = bramCacheDataPipeOut.first;
        bramCacheDataPipeOut.deq;

        dut.write(bramCacheAddr, bramCacheData);
        bramCacheAddrQ.enq(bramCacheAddr);
        bramCacheDataQ.enq(bramCacheData);
        // $display(
        //     "time=%0t:", $time,
        //     " write bramCacheAddr=%h, bramCacheData=%h",
        //     bramCacheAddr, bramCacheData
        // );
    endrule

    rule readBramCache;
        let bramCacheAddr = bramCacheAddrQ.first;
        bramCacheAddrQ.deq;

        // dut.readReq(bramCacheAddr);
        dut.read.request.put(bramCacheAddr);
        // $display(
        //     "time=%0t:", $time,
        //     " read request bramCacheAddr=%h", bramCacheAddr
        // );
    endrule

    rule checkReadResp;
        let bramCacheReadData <- dut.read.response.get;
        // let bramCacheReadData <- dut.readResp;
        let bramCacheReadDataRef = bramCacheDataQ.first;
        bramCacheDataQ.deq;

        immAssert(
            bramCacheReadData == bramCacheReadDataRef,
            "bramCacheReadData assertion @ mkTestBramCache",
            $format(
                "bramCacheReadData=%h should == bramCacheReadDataRef=%h",
                bramCacheReadData, bramCacheReadDataRef
            )
        );
        countDown.decr;
        // $display(
        //     "bramCacheReadData=%h should == bramCacheReadDataRef=%h",
        //     bramCacheReadData, bramCacheReadDataRef
        // );
    endrule
endmodule

(* doc = "testcase" *)
module mkTestTLB(Empty);
    let dut <- mkTLB;

    PipeOut#(ADDR)                             virtAddrPipeOut <- mkGenericRandomPipeOut;
    PipeOut#(Bit#(TLB_CACHE_PA_DATA_WIDTH)) phyAddrDataPipeOut <- mkGenericRandomPipeOut;

    FIFOF#(Tuple2#(ASID,ADDR))                             virtAddrQ <- mkFIFOF;
    FIFOF#(ADDR)                         virtAddrQ4Ref <- mkFIFOF;
    FIFOF#(Bit#(TLB_CACHE_PA_DATA_WIDTH)) phyAddrDataQ <- mkFIFOF;

    let countDown <- mkCountDown(valueOf(MAX_CMP_CNT));

    Reg#(ASID) firstStageIdx <- mkReg(0);
    
    Reg#(Bool) insertFirstOrSecond <- mkReg(True);

    rule insertFirstStage2TLB;
        insertFirstOrSecond <= !insertFirstOrSecond;

        let virtAddr = virtAddrPipeOut.first;
        let phyAddrData = phyAddrDataPipeOut.first;

        let pageOffset = getPageOffset(virtAddr);
        let phyAddr = restorePA(phyAddrData, pageOffset);

        let secondStageOffset = unpack(extend(pack(firstStageIdx))) << 1;
        let firstStageReq = tagged Req4FirstStage PgtModifyFirstStageReq{
            asid: firstStageIdx,
            content: PgtFirstStagePayload {
                secondStageOffset: secondStageOffset,
                secondStageEntryCnt: 2,
                baseVA: getPageAlignedAddr(virtAddr) - fromInteger(valueOf(PAGE_SIZE_CAP))
            }
        };
            
        let secondStageReq = tagged Req4SecondStage PgtModifySecondStageReq{
            index: secondStageOffset+1,
            content: PgtSecondStagePayload {
                paPart: getData4PA(phyAddr)
            }
        };
        if (insertFirstOrSecond) begin
            dut.modify(firstStageReq);
        end else begin
            virtAddrPipeOut.deq;
            phyAddrDataPipeOut.deq;
            dut.modify(secondStageReq);
            virtAddrQ.enq(tuple2(firstStageIdx, virtAddr));
            phyAddrDataQ.enq(phyAddrData);
            firstStageIdx <= firstStageIdx + 1;
        end

        
    endrule


    rule findInTLB;
        let virtAddr = virtAddrQ.first;
        virtAddrQ.deq;

        dut.find.request.put(virtAddr);
        virtAddrQ4Ref.enq(tpl_2(virtAddr));
    endrule

    rule checkFindResp;
        let { foundOrNot, phyAddr } <- dut.find.response.get;
        // let { foundOrNot, phyAddr } <- dut.findResp;
        let phyAddrData = getData4PA(phyAddr);
        let phyAddrDataRef = phyAddrDataQ.first;
        phyAddrDataQ.deq;
        let virtAddrRef = virtAddrQ4Ref.first;
        virtAddrQ4Ref.deq;

        immAssert(
            foundOrNot,
            "foundOrNot assertion @ mkTestTLB",
            $format(
                "foundOrNot=", fshow(foundOrNot), " should be true"
            )
        );

        immAssert(
            phyAddrData == phyAddrDataRef,
            "phyAddrData assertion @ mkTestTLB",
            $format(
                "phyAddrData=%h should == phyAddrDataRef=%h",
                phyAddrData, phyAddrDataRef
            )
        );
        countDown.decr;
        $display(
            "time=%0t:", $time,
            " foundOrNot=", fshow(foundOrNot),
            ", virtAddr=%h v.s. phyAddr=%h",
            virtAddrRef, phyAddr,
            ", phyAddrData=%h should == phyAddrDataRef=%h",
            phyAddrData, phyAddrDataRef
        );
    endrule
endmodule