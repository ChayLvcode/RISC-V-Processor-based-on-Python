import os
import argparse
import struct

MemSize = 1000 # memory size, in reality, the memory size should be 2^32, but for this lab, for the space resaon, we keep it as this large number, but the memory is still 32-bit addressable.

class InsMem(object):
    def __init__(self, name, ioDir):
        self.id = name
        
        with open(ioDir + "\\imem.txt") as im:
            self.IMem = [data.replace("\n", "") for data in im.readlines()]

    def readInstr(self, ReadAddress):
        # Assuming ReadAddress is already a multiple of 4 (which it should be for 32-bit aligned instructions)
        # Extract the 32-bit instruction as a slice from the memory array
        instruction = ''.join(self.IMem[ReadAddress:ReadAddress + 4])
        # Convert the binary string to a hexadecimal value and return
        return hex(int(instruction, 2))
          
class DataMem(object):
    def __init__(self, name, ioDir):
        self.id = name
        self.ioDir = ioDir
        with open(ioDir + "/dmem.txt") as dm:
            self.DMem = [data.replace("\n", "") for data in dm.readlines()]
        numRemain = MemSize-len(self.DMem)
        for i in range(numRemain):
            self.DMem.append('00000000')

    def readInstr(self, ReadAddress):
        # Assuming ReadAddress is already a multiple of 4
        # Read the 32-bit word directly as a slice from the memory array
        word = ''.join(self.DMem[ReadAddress:ReadAddress + 4])
        # Convert the binary string to a hexadecimal value
        return hex(int(word, 2))

    def getImmValue(self, imm):
        # Convert binary string to a signed integer
        # The 'int' function interprets the string as a two's complement number
        return int(imm, 2) if imm[0] == '0' else int(imm, 2) - (1 << len(imm))

    def writeDataMem(self, Address, WriteData):
        # write data into byte addressable memory
        startPos = (Address//4)*4
        # convert hex value to binary value, truncate the 0b, fill with length 32
        BinaryData = bin(int(WriteData,16))[2:].zfill(32)
        for i in range(4):
            self.DMem[startPos+i] = BinaryData[8*i:8*i+8]
        # pass
                     
    def outputDataMem(self):
        resPath = self.ioDir + "/" + self.id + "_DMEMResult.txt"
        with open(resPath, "w") as rp:
            rp.writelines([str(data) + "\n" for data in self.DMem])

class RegisterFile(object):
    def __init__(self, ioDir):
        self.outputFile = ioDir + "RFResult.txt"
        self.Registers = ['00000000000000000000000000000000' for i in range(32)]
    
    def readRF(self, Reg_addr):
        # Fill in
        if Reg_addr>=0 and Reg_addr<32:
            return self.Registers[Reg_addr]
        else:
            raise ValueError("Read Register Number Out Of Bound")
    
    def writeRF(self, Reg_addr, Wrt_reg_data):
        # Fill in
        # Wrt_reg_data is a hex str value
        if Reg_addr>=0 and Reg_addr<32:
            self.Registers[Reg_addr] = Wrt_reg_data
        else:
            raise ValueError("Read Register Number Out Of Bound")
         
    def outputRF(self, cycle):
        op = ["-"*70+"\n", "State of RF after executing cycle:" + str(cycle) + "\n"]
        op.extend([str(val)+"\n" for val in self.Registers])
        if(cycle == 0): perm = "w"
        else: perm = "a"
        with open(self.outputFile, perm) as file:
            file.writelines(op)

    def getSigned32bit(self, hex_str):
        # Convert hex string to a 32-bit integer
        int_value = int(hex_str, 16)
        # Check if the sign bit is set (negative number)
        if int_value & (1 << 31):
            # It's a negative number; the int_value is already in two's complement form due to Python's handling of integers.
            # Mask with 0xffffffff to get the correct 32-bit two's complement representation.
            return bin(int_value & 0xffffffff)[2:].zfill(32)
        else:
            # It's a non-negative number, convert to binary directly
            return bin(int_value)[2:].zfill(32) 

class State(object):
    def __init__(self):
        self.IF = {"nop": False, "PC": 0}
        self.ID = {"nop": True, "Instr": ''}
        self.EX = {"nop": True, "Read_data1": 0, "Read_data2": 0, "Imm": 0, "Rs": 0, "Rt": 0, "Wrt_reg_addr": 0, "is_I_type": False,  "rd_mem": 0, 
                   "wrt_mem": 0, "alu_op": 0, "alu_control":0, "wrt_enable": 0}
        self.MEM = {"nop": True, "ALUresult": 0, "Store_data": 0, "Rs": 0, "Rt": 0, "Wrt_reg_addr": 0, "rd_mem": 0, 
                   "wrt_mem": 0, "wrt_enable": 0}
        self.WB = {"nop": True, "Wrt_data": 0, "Rs": 0, "Rt": 0, "Wrt_reg_addr": 0, "wrt_enable": 0}

class Core(object):
    def __init__(self, ioDir, imem, dmem):
        self.myRF = RegisterFile(ioDir)
        self.cycle = 0
        self.halted = False
        self.ioDir = ioDir
        self.state = State()
        self.nextState = State()
        self.ext_imem = imem
        self.ext_dmem = dmem
        self.numInstructions = 0

    def getImmValue(self, imm):
        # Convert binary string to a signed integer
        # The 'int' function interprets the string as a two's complement number
        return int(imm, 2) if imm[0] == '0' else int(imm, 2) - (1 << len(imm))


class SingleStageCore(Core):
    def __init__(self, ioDir, imem, dmem):
        super(SingleStageCore, self).__init__(ioDir + "/SS_", imem, dmem)
        self.opFilePath = ioDir + "/StateResult_SS.txt"

    def step(self):
        # Your implementation
        self.halted = False

        # IF stage
        self.numInstructions += 1
        # get pc and check if pc value exists
        pc_value = self.state.IF.get("PC")
        if pc_value is None:
            raise ValueError("Program Counter (PC) is not set.")
        # get instruction and check if we can read the instruction
        hex_instruction = self.ext_imem.readInstr(pc_value)
        if hex_instruction is None:
            raise ValueError("No instruction found at PC: {pc_value}")
        # get the 16bit instu to int and change to 32bit bin
        int_instruction = int(hex_instruction, 16)
        instruction = bin(int_instruction)[2:].zfill(32)

        HALT_OPCODE = '1111111'
        # PC + 4
        next_pc = pc_value + 4
        if pc_value<len(self.ext_imem.IMem)-4 and instruction[25:32] != HALT_OPCODE:
            self.nextState.IF["PC"] = next_pc
        else:
            self.nextState.IF["PC"] = pc_value

        
        # R-type OPcode[6:0], we get rs1 and rs2 for convient
        if instruction[25:32] == '0110011':
            rs2 = int(instruction[7:12],2)
            rs1 = int(instruction[12:17],2)
            rd = int(instruction[20:25],2)
            if instruction[17:20] == '000':
                # add
                if instruction[0:7] == '0000000':
                    addResult = int(self.myRF.readRF(rs2),16) + int(self.myRF.readRF(rs1),16)
                    addResult = hex(addResult)
                    self.myRF.writeRF(rd,addResult)
                # sub
                else:
                    subResult = int(self.myRF.readRF(rs1),16) - int(self.myRF.readRF(rs2),16)
                    subResult = hex(subResult)
                    self.myRF.writeRF(rd,subResult)
            # xor
            elif instruction[17:20] == '100':
                result = int(self.myRF.readRF(rs2),16)^int(self.myRF.readRF(rs1),16)
                self.myRF.writeRF(rd, hex(result))
            # or
            elif instruction[17:20] == '110':
                result = int(self.myRF.readRF(rs2),16)|int(self.myRF.readRF(rs1),16)
                self.myRF.writeRF(rd,hex(result))
            # and
            elif instruction[17:20] == '111':
                result = int(self.myRF.readRF(rs2),16)&int(self.myRF.readRF(rs1),16)
                self.myRF.writeRF(rd,hex(result))

        # I-type OPcode[6:0]
        elif instruction[25:32] == '0010011':
            imm = self.getImmValue(instruction[0:12])
            rs1 = int(instruction[12:17],2)
            rd = int(instruction[20:25],2)
            # addi
            if instruction[17:20] == '000':
                result = int(self.myRF.readRF(rs1),16) + imm
                self.myRF.writeRF(rd, hex(result))
            # xori
            elif instruction[17:20] == '100':
                result = int(self.myRF.readRF(rs1),16) ^ imm
                self.myRF.writeRF(rd, hex(result))
            # ori
            elif instruction[17:20] == '110':
                result = int(self.myRF.readRF(rs1),16) | imm
                self.myRF.writeRF(rd, hex(result))
            # andi
            elif instruction[17:20] == '111':
                result = int(self.myRF.readRF(rs1),16) & imm
                self.myRF.writeRF(rd, hex(result))

        # JAL instruction[6:0]
        elif instruction[25:32] == '1101111':
            # get Imm value here
            imm = self.getImmValue(instruction[0]+instruction[12:19]+instruction[11]+instruction[1:11]+'0')
            rd = int(instruction[20:25],2)
            self.myRF.writeRF(rd, hex(self.state.IF["PC"]+4))
            self.nextState.IF["PC"] = self.state.IF["PC"] + imm

        # B-type instruction[6:0]
        elif instruction[25:32] == '1100011':
            # get Imm value here
            imm = self.getImmValue(instruction[0]+instruction[24]+instruction[1:7]+instruction[20:24]+'0')
            rs2 = int(instruction[7:12],2)
            rs1 = int(instruction[12:17],2)
            # BEQ
            if instruction[17:20] == '000':
                if self.myRF.readRF(rs1)==self.myRF.readRF(rs2):
                    self.nextState.IF["PC"] = self.state.IF["PC"] + imm
            # BEN
            elif instruction[17:20] == '001':
                if self.myRF.readRF(rs1)!=self.myRF.readRF(rs2):
                    self.nextState.IF["PC"] = self.state.IF["PC"] + imm

        # LW instruction
        elif instruction[25:32] == '0000011':
            imm = self.getImmValue(instruction[0:12])
            rs1 = int(instruction[12:17],2)
            rd = int(instruction[20:25],2)
            self.myRF.writeRF(rd,self.ext_dmem.readInstr(int(self.myRF.readRF(rs1),16) + imm))
            
        # SW instruction
        elif instruction[25:32] == '0100011':
            imm = instruction[0:7]+instruction[20:25]
            imm = self.getImmValue(imm)
            rs2 = int(instruction[7:12],2)
            rs1 = int(instruction[12:17],2)
            self.ext_dmem.writeDataMem(int(self.myRF.readRF(rs1),16) + imm, hex(int(self.myRF.readRF(rs2),16)))
           
        # Halt
        elif instruction[25:32] == '1111111':
            self.nextState.IF["nop"] = True
            

        #-----------------------
        if self.state.IF["nop"]:
            self.halted = True
            
        self.myRF.outputRF(self.cycle) # dump RF
        self.printState(self.nextState, self.cycle) # print states after executing cycle 0, cycle 1, cycle 2 ... 
            
        self.state = self.nextState #The end of the cycle and updates the current state with the values calculated in this cycle
        self.nextState = State() #Brand New State
        self.cycle += 1
        
    def printState(self, state, cycle):
        printstate = ["-"*70+"\n", "State after executing cycle: " + str(cycle) + "\n"]
        printstate.append("IF.PC: " + str(state.IF["PC"]) + "\n")
        printstate.append("IF.nop: " + str(state.IF["nop"]) + "\n")
        
        if(cycle == 0): perm = "w"
        else: perm = "a"
        with open(self.opFilePath, perm) as wf:
            wf.writelines(printstate)


class FiveStageCore(Core):
    def __init__(self, ioDir, imem, dmem):
        super(FiveStageCore, self).__init__(ioDir + "\\FS_", imem, dmem)
        self.opFilePath = ioDir + "\\StateResult_FS.txt"

    def step(self):
        # Your implementation
        # --------------------- WB stage ---------------------
        
        
        
        # --------------------- MEM stage --------------------
        
        
        
        # --------------------- EX stage ---------------------
        
        
        
        # --------------------- ID stage ---------------------
        
        
        
        # --------------------- IF stage ---------------------
        
        self.halted = True
        if self.state.IF["nop"] and self.state.ID["nop"] and self.state.EX["nop"] and self.state.MEM["nop"] and self.state.WB["nop"]:
            self.halted = True
        
        self.myRF.outputRF(self.cycle) # dump RF
        self.printState(self.nextState, self.cycle) # print states after executing cycle 0, cycle 1, cycle 2 ... 
        
        self.state = self.nextState #The end of the cycle and updates the current state with the values calculated in this cycle
        self.nextState = State()
        self.cycle += 1

    def printState(self, state, cycle):
        printstate = ["-"*70+"\n", "State after executing cycle: " + str(cycle) + "\n"]
        printstate.extend(["IF." + key + ": " + str(val) + "\n" for key, val in state.IF.items()])
        printstate.extend(["ID." + key + ": " + str(val) + "\n" for key, val in state.ID.items()])
        printstate.extend(["EX." + key + ": " + str(val) + "\n" for key, val in state.EX.items()])
        printstate.extend(["MEM." + key + ": " + str(val) + "\n" for key, val in state.MEM.items()])
        printstate.extend(["WB." + key + ": " + str(val) + "\n" for key, val in state.WB.items()])

        if(cycle == 0): perm = "w"
        else: perm = "a"
        with open(self.opFilePath, perm) as wf:
            wf.writelines(printstate)

if __name__ == "__main__": 
    #parse arguments for input file location
    parser = argparse.ArgumentParser(description='RV32I processor')
    parser.add_argument('--iodir', default="", type=str, help='Directory containing the input files.')
    args = parser.parse_args()

    ioDir = os.path.abspath(args.iodir)
    print("IO Directory:", ioDir)

    imem = InsMem("Imem", ioDir)
    dmem_ss = DataMem("SS", ioDir)
    dmem_fs = DataMem("FS", ioDir)
    
    ssCore = SingleStageCore(ioDir, imem, dmem_ss)
    fsCore = FiveStageCore(ioDir, imem, dmem_fs)

    while(True):
        if not ssCore.halted:
            ssCore.step()
        
        if not fsCore.halted:
            fsCore.step()

        if ssCore.halted and fsCore.halted:
            # print Performance Metrics Result
            resPath = ioDir + "/" + "PerformanceMetrics_result.txt"
            with open(resPath, "w") as rp:
                rp.writelines("Performance of Single Stage:\n")
                rp.writelines("#Cycles -> "+str(ssCore.cycle)+"\n")
                rp.writelines("#Instructions -> "+str(ssCore.numInstructions)+"\n")
                rp.writelines("CPI -> "+str(ssCore.cycle/(ssCore.numInstructions-1))+"\n")
                rp.writelines("IPC -> "+str((ssCore.numInstructions-1)/ssCore.cycle)+"\n\n\n")
            break
    
    # dump SS and FS data mem.
    dmem_ss.outputDataMem()
    dmem_fs.outputDataMem()