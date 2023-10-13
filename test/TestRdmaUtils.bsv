import RdmaUtils :: *;
import PrimUtils :: *;
import DataTypes :: *;
import Headers :: *;
import Vector :: *;

(* doc = "testcase" *)
module mkTestCalcFirstPktLenFromAddrAndPMTU(Empty) provisos (NumAlias#(10, testCaseCnt));

    Vector#(testCaseCnt, Tuple3#(ADDR, PMTU, PktLen)) testCase = newVector;

    testCase[0] = tuple3(64'hFFFFFFFFFFFFFFFF, IBV_MTU_256, 13'h0001);
    testCase[1] = tuple3(64'hFFFFFFFFFFFFFFFF, IBV_MTU_512, 13'h0001);
    testCase[2] = tuple3(64'hFFFFFFFFFFFFFFFF, IBV_MTU_1024, 13'h0001);
    testCase[3] = tuple3(64'hFFFFFFFFFFFFFFFF, IBV_MTU_2048, 13'h0001);
    testCase[4] = tuple3(64'hFFFFFFFFFFFFFFFF, IBV_MTU_4096, 13'h0001);

    testCase[5] = tuple3(64'hFFFFFFFFFFFF0000, IBV_MTU_256, 13'h0100);
    testCase[6] = tuple3(64'hFFFFFFFFFFFF0000, IBV_MTU_512, 13'h0200);
    testCase[7] = tuple3(64'hFFFFFFFFFFFF0000, IBV_MTU_1024, 13'h0400);
    testCase[8] = tuple3(64'hFFFFFFFFFFFF0000, IBV_MTU_2048, 13'h0800);
    testCase[9] = tuple3(64'hFFFFFFFFFFFF0000, IBV_MTU_4096, 13'h1000);
    
    rule run;
        for (Integer i = 0; i < valueOf(testCaseCnt); i = i + 1) begin 
            let {addr, pmtu, exp} = testCase[fromInteger(i)];
            let ret = calcFirstPktLenFromAddrAndPMTU(addr, pmtu);
            immAssert(
                 ret == exp,
                "calcFirstPktLenFromAddrAndPMTU assertion @ mkTestCalcFirstPktLenFromAddrAndPMTU",
                $format(
                    "when addr = %h and pmtu = %h, output should == %h, but got %h",
                    addr, pmtu, exp, ret
                )
            );
        end
        $finish;
    endrule
endmodule