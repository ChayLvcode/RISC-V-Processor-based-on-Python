import os
import argparse
import struct

MemSize = 1000 # memory size, in reality, the memory size should be 2^32, but for this lab, for the space resaon, we keep it as this large number, but the memory is still 32-bit addressable.

class InsMem(object):
    def __init__(self, name, ioDir):
        self.id = name

        with open(ioDir + "/imem.txt") as im:
            self.IMem = [data.replace("\n", "") for data in im.readlines()]

    def readInstr(self, ReadAddress):
        # Read instruction memory
        # Returns a 32-bit hexadecimal value

        # Calculate the starting position, ensuring it is aligned to a multiple of 4,
        # as each instruction is 4 bytes aligned
        startPos = (ReadAddress // 4) * 4

        # Retrieve the consecutive 4 bytes (32 bits) starting from the calculated position
        content = []
        for i in range(4):
            # Append each byte to the content list
            content.append(self.IMem[startPos + i])

        # Combine the binary string content into one string, convert it to an integer,
        # and then convert to a hexadecimal string
        instruction = ''.join(content)
        instruction_hex = hex(int(instruction, 2))

        return instruction_hex
          
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
        # Read data memory
        # Returns a 32-bit hexadecimal value

        # Calculate the starting position, ensuring it's aligned to a multiple of 4
        # This alignment is necessary as memory addresses are 4 bytes aligned
        startPos = (ReadAddress // 4) * 4

        # Retrieve 4 bytes (32 bits) starting from the calculated position
        content = []
        for i in range(4):
            # Append each byte from data memory to the content list
            content.append(self.DMem[startPos + i])

        # Convert the binary string content to an integer and then to a hexadecimal string
        # 'getImmValue' is used to handle signed integer conversion, assuming it's defined elsewhere
        instruction_hex = hex(self.getImmValue(''.join(content)))

        return instruction_hex

    def getImmValue(self, imm):
        # Extend the immediate (imm) value to a 32-bit binary string
        # Then convert it to a signed integer using two's complement

        # Initialize result to 0
        result = 0

        # Iterate through each bit of the immediate value
        for i in range(len(imm)):
            # For the most significant bit (MSB)
            if i == 0:
                # If MSB is 1, it contributes a negative value (two's complement)
                result += -(2**(len(imm)-1))*int(imm[i])
            else:
                # For other bits, compute the value normally
                result += 2**(len(imm)-1-i)*int(imm[i])

        return result

    def writeDataMem(self, Address, WriteData):
        # Write data into byte-addressable memory
        # Address is aligned to a multiple of 4 as memory is word (4 bytes) addressable

        # Calculate the starting position in memory, aligning to 4-byte boundaries
        startPos = (Address // 4) * 4

        # Convert the hexadecimal value to a binary string, 
        # strip the '0b' prefix, and fill to ensure it is 32 bits long
        BinaryData = bin(int(WriteData, 16))[2:].zfill(32)

        # Write the binary data into memory, one byte at a time
        # Each byte is 8 bits, so we slice the 32-bit binary data into 4 chunks of 8 bits each
        for i in range(4):
            self.DMem[startPos + i] = BinaryData[8 * i : 8 * (i + 1)]
                     
    def outputDataMem(self):
        resPath = self.ioDir + "/" + self.id + "_DMEMResult.txt"
        with open(resPath, "w") as rp:
            rp.writelines([str(data) + "\n" for data in self.DMem])

class RegisterFile(object):
    def __init__(self, ioDir):
        self.outputFile = ioDir + "RFResult.txt"
        self.Registers = ['0x00000000' for i in range(32)]
    
    def readRF(self, Reg_addr):
        # Fill in
        if Reg_addr>=0 and Reg_addr<32:
            return self.Registers[Reg_addr]
        else:
            print("Error: Read Register Number Out Of Bound -", Reg_addr)
        
    
    def writeRF(self, Reg_addr, Wrt_reg_data):
        # Fill in
        # Wrt_reg_data is a hex str value
        if 0 <= Reg_addr < 32:
            self.Registers[Reg_addr] = Wrt_reg_data
        else:
            print("Error: Write Register Number Out Of Bound -", Reg_addr)

         
    def outputRF(self, cycle):
        op = ["-"*70+"\n", "State of RF after executing cycle:" + str(cycle) + "\n"]
        op.extend([bin(int(val, 16))[2:].zfill(32) + "\n" for val in self.Registers])
        if cycle == 0: 
            perm = "w"
        else: 
            perm = "a"
        with open(self.outputFile, perm) as file:
            file.writelines(op)

    def getSigned32bit(self, hex_str):
        # Convert a hexadecimal string to a signed 32-bit binary string representation

        # Convert the hex string to an integer
        int_value = int(hex_str, 16)

        # Check if the integer is non-negative
        if int_value >= 0:
            # Directly convert the integer to a binary string and pad to 32 bits
            return bin(int_value)[2:].zfill(32)
        else:
            # If the integer is negative, apply a mask to get the two's complement representation
            # The mask (0xffffffff) ensures the representation fits into 32 bits
            return bin(int_value & 0xffffffff)[2:].zfill(32)

class State(object):
    def __init__(self):
        self.IF = {"nop": False, "PC": 0}
        self.ID = {"nop": True, "Instr": ''}
        self.EX = {"nop": True, "Read_data1": 0, "Read_data2": 0, "Imm": 0, "Rs": 0, "Rt": 0, "Wrt_reg_addr": 0, "is_I_type": 0,  "rd_mem": 0, 
                   "wrt_mem": 0, "alu_op": 0, "alu_control":0, "wrt_enable": 0}
        self.MEM = {"nop": True, "ALUresult": 0, "Store_data": 0, "Rs": 0, "Rt": 0, "Wrt_reg_addr": 0, "rd_mem": 0, 
                   "wrt_mem": 0, "wrt_enable": 0}
        self.WB = {"nop": True, "Wrt_data": 0, "Rs": 0, "Rt": 0, "Wrt_reg_addr": 0, "wrt_enable": 0}

class Core(object):
    def __init__(self, ioDir, imem, dmem):
        self.myRF = RegisterFile(ioDir)
        self.cycle = 0
        self.numInstructions = 0
        self.halted = False
        self.ioDir = ioDir
        self.state = State()
        self.nextState = State()
        self.ext_imem = imem
        self.ext_dmem = dmem

    def getImmValue(self, imm):
        # Convert a binary string immediate value (imm) to a signed integer
        # The binary string is interpreted as a two's complement number

        # Initialize the result variable
        result = 0

        # Iterate through each bit in the binary string
        for i in range(len(imm)):
            # For the most significant bit (MSB, at index 0)
            if i == 0:
                # If MSB is '1', it represents a negative value in two's complement
                result -= (2 ** (len(imm) - 1)) * int(imm[i])
            else:
                # For other bits, calculate their positive contribution to the total value
                result += (2 ** (len(imm) - 1 - i)) * int(imm[i])

        return result
    

class SingleStageCore(Core):
    def __init__(self, ioDir, imem, dmem):
        super(SingleStageCore, self).__init__(os.path.join(ioDir, "SS_"), imem, dmem)
        self.opFilePath = os.path.join(ioDir, "StateResult_SS.txt")

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

def format_binary(value, bit_length=32):
        return format(value, '0' + str(bit_length) + 'b')

class FiveStageCore(Core):
    def __init__(self, ioDir, imem, dmem):
        super(FiveStageCore, self).__init__(ioDir + "/FS_", imem, dmem)
        self.opFilePath = ioDir + "/StateResult_FS.txt"
        self.endOfFile = False

    def step(self):
        # Your implementation
        # --------------------- WB (Write Back) stage ---------------------
        # Check if the Write Back stage should be executed (i.e., not a 'nop' operation)
        if self.state.WB["nop"] == False:
            # If writing back to a register is enabled
            if self.state.WB["wrt_enable"] == 1:
                # Write the computed data back to the specified register
                self.myRF.writeRF(self.state.WB["Wrt_reg_addr"], hex(self.state.WB["Wrt_data"]))
        # --------------------- MEM stage --------------------
        # Passing 'nop' (no operation) flag from MEM to WB stage
        self.nextState.WB["nop"] = self.state.MEM["nop"]

        # If MEM stage is not a 'nop', proceed with memory operations
        if self.state.MEM["nop"] == False:
            # Pass write enable flag and destination register address to WB stage
            self.nextState.WB["wrt_enable"] = self.state.MEM["wrt_enable"]
            self.nextState.WB["Wrt_reg_addr"] = self.state.MEM["Wrt_reg_addr"]

            # If read from memory is required (LW instruction)
            if self.state.MEM["rd_mem"] == 1:
                # Read data from memory and set it to be written in the WB stage
                readData = int(self.ext_dmem.readInstr(self.state.MEM["ALUresult"]), 16)
                self.nextState.WB["Wrt_data"] = readData
                # Pass source and target registers info to WB stage
                self.nextState.WB["Rs"] = self.state.MEM["Rs"]
                self.nextState.WB["Rt"] = self.state.MEM["Rt"]

            # If write to memory is required (SW instruction)
            elif self.state.MEM["wrt_mem"] == 1:
                # Write data to memory at the address given by ALUresult
                writeAddress = self.state.MEM["ALUresult"]
                dataToWrite = hex(self.state.MEM["Store_data"])
                self.ext_dmem.writeDataMem(writeAddress, dataToWrite)

            # For other types of instructions (R-type, I-type) that do not interact with memory
            else:
                # Pass ALU result directly to WB stage
                self.nextState.WB["Wrt_data"] = self.state.MEM["ALUresult"]
        # --------------------- EX stage ---------------------
        self.nextState.MEM["nop"] = self.state.EX["nop"]

        # Proceed if not a 'nop' (no operation)
        if self.state.EX["nop"] == False:
            # Handling R-type instructions (alu_op == 0b10)
            if self.state.EX["alu_op"] == 0b10:
                # Depending on the ALU control signal, perform the appropriate operation

                # ADD operation
                if self.state.EX["alu_control"] == 0b0010:
                    self.nextState.MEM["ALUresult"] = self.state.EX["Read_data1"] + self.state.EX["Read_data2"]

                # SUBTRACT operation
                elif self.state.EX["alu_control"] == 0b0110:
                    self.nextState.MEM["ALUresult"] = self.state.EX["Read_data1"] - self.state.EX["Read_data2"]

                # XOR operation
                elif self.state.EX["alu_control"] == 0b1111:
                    self.nextState.MEM["ALUresult"] = self.state.EX["Read_data1"] ^ self.state.EX["Read_data2"]

                # OR operation
                elif self.state.EX["alu_control"] == 0b0001:
                    self.nextState.MEM["ALUresult"] = self.state.EX["Read_data1"] | self.state.EX["Read_data2"]

                # AND operation
                elif self.state.EX["alu_control"] == 0b0000:
                    self.nextState.MEM["ALUresult"] = self.state.EX["Read_data1"] & self.state.EX["Read_data2"]

                # Common settings for R-type instructions
                self.nextState.MEM["Rs"] = self.state.EX["Rs"]
                self.nextState.MEM["Rt"] = self.state.EX["Rt"]
                self.nextState.MEM["Store_data"] = 0
                self.nextState.MEM["Wrt_reg_addr"] = self.state.EX["Wrt_reg_addr"]
                self.nextState.MEM["rd_mem"] = 0
                self.nextState.MEM["wrt_mem"] = 0
                self.nextState.MEM["wrt_enable"] = 1


            elif self.state.EX["alu_op"] == 0b00:  # I-type, LW and SW instructions
                # No memory operation (ADDI, XORI, ORI, ANDI)
                if self.state.EX["wrt_mem"] == 0 and self.state.EX["rd_mem"] == 0:
                    # ADDI operation
                    if self.state.EX["alu_control"] == 0b0010:
                        self.nextState.MEM["ALUresult"] = self.state.EX["Read_data1"] + self.state.EX["Imm"]

                    # XORI operation
                    elif self.state.EX["alu_control"] == 0b1111:
                        self.nextState.MEM["ALUresult"] = self.state.EX["Read_data1"] ^ self.state.EX["Imm"]

                    # ORI operation
                    elif self.state.EX["alu_control"] == 0b0001:
                        self.nextState.MEM["ALUresult"] = self.state.EX["Read_data1"] | self.state.EX["Imm"]

                    # ANDI operation
                    elif self.state.EX["alu_control"] == 0b0000:
                        self.nextState.MEM["ALUresult"] = self.state.EX["Read_data1"] & self.state.EX["Imm"]

                    # Common settings for I-type arithmetic instructions
                    self.nextState.MEM["Rs"] = self.state.EX["Rs"]
                    self.nextState.MEM["Rt"] = 0  # Rt is not used in I-type arithmetic instructions
                    self.nextState.MEM["Store_data"] = 0  # No data to store
                    self.nextState.MEM["Wrt_reg_addr"] = self.state.EX["Wrt_reg_addr"]
                    self.nextState.MEM["rd_mem"] = 0  # No memory read
                    self.nextState.MEM["wrt_mem"] = 0  # No memory write
                    self.nextState.MEM["wrt_enable"] = 1  # Enable writing back to register

                elif self.state.EX["rd_mem"] == 1 and self.state.EX["wrt_enable"] == 1:
                    # Calculate memory address for loading data
                    self.nextState.MEM["ALUresult"] = self.state.EX["Read_data1"] + self.state.EX["Imm"]
                    self.nextState.MEM["Rs"] = self.state.EX["Rs"]
                    self.nextState.MEM["Rt"] = 0  # Rt is not used in LW instruction
                    self.nextState.MEM["Store_data"] = 0  # No data to store in memory
                    self.nextState.MEM["Wrt_reg_addr"] = self.state.EX["Wrt_reg_addr"]  # Register to write data into
                    self.nextState.MEM["rd_mem"] = 1  # Flag indicating a memory read operation
                    self.nextState.MEM["wrt_mem"] = 0  # No memory write operation
                    self.nextState.MEM["wrt_enable"] = 1  # Enable writing data back to register

                # Handling Store Word (SW) instruction
                elif self.state.EX["wrt_mem"] == 1:
                    # Calculate memory address for storing data
                    self.nextState.MEM["ALUresult"] = self.state.EX["Read_data1"] + self.state.EX["Imm"]
                    self.nextState.MEM["Rs"] = self.state.EX["Rs"]
                    self.nextState.MEM["Rt"] = 0  # Rt is not used in SW instruction
                    self.nextState.MEM["Store_data"] = self.state.EX["Read_data2"]  # Data to be stored in memory
                    self.nextState.MEM["Wrt_reg_addr"] = 0  # No register writing in SW
                    self.nextState.MEM["rd_mem"] = 0  # No memory read operation
                    self.nextState.MEM["wrt_mem"] = 1  # Flag indicating a memory write operation
                    self.nextState.MEM["wrt_enable"] = 0  # No writing data back to register

            else: #branch option: if branch condition not met, (jal, beq, bne), continue pipeline, no mem op, no wb op.
                self.nextState.MEM["rd_mem"] = 0 
                self.nextState.MEM["wrt_mem"] = 0
                self.nextState.MEM["wrt_enable"] = 0
        
        # --------------------- ID stage ---------------------
        # Transition from ID (Instruction Decode) stage to EX (Execute) stage
        self.nextState.EX["nop"] = self.state.ID["nop"]

        # Proceed if not a 'nop' (no operation)
        if self.state.ID["nop"] == False:
            # Fetch the instruction from the ID stage
            instruction = self.state.ID["Instr"]  # Binary string of the instruction

            # Handling R-type instructions (opcode 0110011)
            if instruction[25:32] == '0110011':
                # Extract the source registers (Rs, Rt) and the destination register (Wrt_reg_addr)
                self.nextState.EX["Rs"] = int(instruction[12:17], 2)
                self.nextState.EX["Rt"] = int(instruction[7:12], 2)
                self.nextState.EX["Wrt_reg_addr"] = int(instruction[20:25], 2)

                # Indicate that the instruction is not an I-type
                self.nextState.EX["is_I_type"] = 0

                # Read data from the register file based on the source registers
                self.nextState.EX["Read_data1"] = int(self.myRF.readRF(int(instruction[12:17], 2)), 16)
                self.nextState.EX["Read_data2"] = int(self.myRF.readRF(int(instruction[7:12], 2)), 16)

                # Set flags for memory read/write and write-back enable
                self.nextState.EX["rd_mem"] = 0  # No memory read for R-type
                self.nextState.EX["wrt_mem"] = 0  # No memory write for R-type
                self.nextState.EX["wrt_enable"] = 1  # Enable writing back to register

                # Set the ALU operation type for R-type
                self.nextState.EX["alu_op"] = 0b10
                
                # ADD or SUB operation
                if instruction[17:20] == '000':
                    # ADD operation
                    if instruction[0:7] == '0000000':
                        self.nextState.EX["alu_control"] = 0b0010  # ALU control signal for ADD
                    # SUB operation
                    else:
                        self.nextState.EX["alu_control"] = 0b0110  # ALU control signal for SUB
                # XOR operation
                elif instruction[17:20] == '100':
                    self.nextState.EX["alu_control"] = 0b1111  # ALU control signal for XOR

                # OR operation
                elif instruction[17:20] == '110':
                    self.nextState.EX["alu_control"] = 0b0001  # ALU control signal for OR

                # AND operation
                elif instruction[17:20] == '111':
                    self.nextState.EX["alu_control"] = 0b0000  # ALU control signal for AND

            # Check if it's an I-type instruction
            if instruction[25:32] == '0010011':
                # Extract the immediate value
                imm = self.getImmValue(instruction[0:12])
                self.nextState.EX["Imm"] = imm

                # Extract the source register (Rs) and set the target register (Rt) to 0 (unused in I-type)
                self.nextState.EX["Rs"] = int(instruction[12:17], 2)
                self.nextState.EX["Rt"] = 0

                # Extract the destination register address (Wrt_reg_addr)
                self.nextState.EX["Wrt_reg_addr"] = int(instruction[20:25], 2)

                # Indicate that this is an I-type instruction
                self.nextState.EX["is_I_type"] = 1

                # Read data from the register file based on the source register (Rs)
                self.nextState.EX["Read_data1"] = int(self.myRF.readRF(int(instruction[12:17], 2)), 16)

                # Set the second read data to 0 as it's not used in I-type instructions
                self.nextState.EX["Read_data2"] = 0

                # Set flags for memory read/write and write-back enable
                self.nextState.EX["rd_mem"] = 0  # No memory read in I-type arithmetic instructions
                self.nextState.EX["wrt_mem"] = 0  # No memory write in I-type arithmetic instructions
                self.nextState.EX["wrt_enable"] = 1  # Enable writing back to the register

                # Set the ALU operation type for I-type
                self.nextState.EX["alu_op"] = 0b00
                # ADDI operation
                if instruction[17:20] == '000':
                    # Set the ALU control signal for ADDI (add immediate)
                    self.nextState.EX["alu_control"] = 0b0010

                # XORI operation
                elif instruction[17:20] == '100':
                    # Set the ALU control signal for XORI (xor immediate)
                    self.nextState.EX["alu_control"] = 0b1111

                # ORI operation
                elif instruction[17:20] == '110':
                    # Set the ALU control signal for ORI (or immediate)
                    self.nextState.EX["alu_control"] = 0b0001
                    
            # Check if it's a JAL instruction
            if instruction[25:32] == '1101111':
                # Reconstruct the immediate value from the instruction
                imm = instruction[0] + instruction[12:19] + instruction[11] + instruction[1:11] + '0'
                imm = self.getImmValue(imm)

                # Set the next EX stage to nop since JAL is a control instruction and disrupts normal flow
                self.nextState.EX["nop"] = True

                # Clearing and setting necessary fields for JAL
                self.nextState.EX["Imm"] = 0
                self.nextState.EX["Rs"] = 0
                self.nextState.EX["Rt"] = 0
                self.nextState.EX["Wrt_reg_addr"] = int(instruction[20:25], 2)
                self.nextState.EX["is_I_type"] = 0
                self.nextState.EX["Read_data1"] = 0  # Typically the address of the next instruction (PC+4)
                self.nextState.EX["Read_data2"] = 0
                self.nextState.EX["rd_mem"] = 0
                self.nextState.EX["wrt_mem"] = 0
                self.nextState.EX["wrt_enable"] = 0
                self.nextState.EX["alu_op"] = 0b11  # JAL operation

                # Setting the ID stage to nop to prevent fetching new instructions
                self.nextState.ID["nop"] = True

                # Calculating the new PC value based on the immediate value and the current PC
                if self.endOfFile:
                    self.nextState.IF["PC"] = imm + self.state.IF["PC"]
                else:
                    self.nextState.IF["PC"] = imm + self.state.IF["PC"] - 4

                # Write the return address (PC+4) to the specified register
                self.myRF.writeRF(self.nextState.EX["Wrt_reg_addr"], hex(self.state.IF["PC"]))
                
            # U-type instruction[6:0]
            if instruction[25:32] == '1100011':
                # Reconstruct the immediate value for the branch instruction
                imm = instruction[0] + instruction[24] + instruction[1:7] + instruction[20:24] + '0'
                imm = self.getImmValue(imm)

                # Setting the immediate value to 0 as it's used for calculating branch target
                self.nextState.EX["Imm"] = imm

                # Extract the source registers (Rs and Rt)
                self.nextState.EX["Rs"] = int(instruction[12:17], 2)
                self.nextState.EX["Rt"] = int(instruction[7:12], 2)

                # No write-back to register for branch instructions
                self.nextState.EX["Wrt_reg_addr"] = 0
                self.nextState.EX["is_I_type"] = 0

                # Read data from the register file based on the source registers
                self.nextState.EX["Read_data1"] = int(self.myRF.readRF(int(instruction[12:17], 2)), 16)
                self.nextState.EX["Read_data2"] = int(self.myRF.readRF(int(instruction[7:12], 2)), 16)

                # Set flags for memory read/write and write-back enable
                self.nextState.EX["rd_mem"] = 0  # No memory read in branch instructions
                self.nextState.EX["wrt_mem"] = 0  # No memory write in branch instructions
                self.nextState.EX["wrt_enable"] = 0  # No writing back to register

                # Set the ALU operation type for Branch
                self.nextState.EX["alu_op"] = 0b01
                # ------------------- Hazard Examination -----------------
                # Check if the current EX stage instruction writes to a register
                if self.state.EX["wrt_enable"] == 1:
                    # Handling R-type instructions
                    if self.state.EX["rd_mem"] == 0:
                        # Forward data from MEM to EX if Rs of the next instruction matches Wrt_reg_addr of the current instruction
                        if self.nextState.EX["Rs"] == self.state.EX["Wrt_reg_addr"] and self.nextState.EX["Rs"] != 0:
                            self.nextState.EX["Read_data1"] = self.nextState.MEM["ALUresult"]
                        
                        # Forward data from MEM to EX if Rt of the next instruction matches Wrt_reg_addr of the current instruction
                        elif self.nextState.EX["Rt"] == self.state.EX["Wrt_reg_addr"] and self.nextState.EX["Rt"] != 0:
                            self.nextState.EX["Read_data2"] = self.nextState.MEM["ALUresult"]

                    # Handling Load-use-data hazard for LW instruction
                    elif self.state.EX["rd_mem"] == 1:
                        # Check for hazard condition: Rs or Rt of the next instruction matches Wrt_reg_addr of the current instruction
                        if (self.nextState.EX["Rs"] == self.state.EX["Wrt_reg_addr"] and self.nextState.EX["Rs"] != 0) \
                            or (self.nextState.EX["Rt"] == self.state.EX["Wrt_reg_addr"] and self.nextState.EX["Rt"] != 0):
                            
                            # Introduce a stall (bubble) in the pipeline
                            self.nextState.EX["nop"] = True   # Next cycle, ALU doesn't do anything; MEM proceeds as usual
                            self.nextState.EX["wrt_enable"] = 0 # Flush signals to avoid re-detecting the hazard
                            self.nextState.EX["wrt_mem"] = 0
                            self.nextState.EX["rd_mem"] = 0
                            
                            # Adjust PC to reload the same instruction (handling the stall)
                            self.state.IF["PC"] = self.state.IF["PC"] - 4
                            
                # MEM/WB forwarding to handle data hazards
                # Check if the MEM stage instruction writes to a register
                if self.state.MEM["wrt_enable"] == 1:
                    # Forward data from WB to EX if Rs of the next instruction matches Wrt_reg_addr of the MEM stage instruction
                    if self.nextState.EX["Rs"] == self.state.MEM["Wrt_reg_addr"] and self.nextState.EX["Rs"] != 0:
                        self.nextState.EX["Read_data1"] = self.nextState.WB["Wrt_data"]

                    # Forward data from WB to EX if Rt of the next instruction matches Wrt_reg_addr of the MEM stage instruction
                    elif self.nextState.EX["Rt"] == self.state.MEM["Wrt_reg_addr"] and self.nextState.EX["Rt"] != 0:
                        self.nextState.EX["Read_data2"] = self.nextState.WB["Wrt_data"]
                #-------------------- End of Hazard Examination ------------
                # BEQ (Branch if Equal) instruction
                if instruction[17:20] == '000':
                    # Check if the values in the two source registers are equal
                    if self.nextState.EX["Read_data1"] == self.nextState.EX["Read_data2"]:
                        # Introduce a stall in the ID stage (nop)
                        self.nextState.ID["nop"] = True

                        # Calculate the new PC based on the immediate value
                        if self.endOfFile:
                            self.nextState.IF["PC"] = imm + self.state.IF["PC"]
                        else:
                            self.nextState.IF["PC"] = imm + self.state.IF["PC"] - 4

                # BNE (Branch if Not Equal) instruction
                elif instruction[17:20] == '001':
                    # Check if the values in the two source registers are not equal
                    if self.nextState.EX["Read_data1"] != self.nextState.EX["Read_data2"]:
                        # Introduce a stall in the ID stage (nop)
                        self.nextState.ID["nop"] = True

                        # Calculate the new PC based on the immediate value
                        if self.endOfFile:
                            self.nextState.IF["PC"] = imm + self.state.IF["PC"]
                        else:
                            self.nextState.IF["PC"] = imm + self.state.IF["PC"] - 4

            # Check if it's an LW instruction
            if instruction[25:32] == '0000011':
                # Extract the immediate value
                imm = self.getImmValue(instruction[0:12])
                self.nextState.EX["Imm"] = imm

                # Extract the base address source register (Rs) and set Rt as 0 (unused in LW)
                self.nextState.EX["Rs"] = int(instruction[12:17], 2)
                self.nextState.EX["Rt"] = 0

                # Extract the destination register address (Wrt_reg_addr) where data will be loaded
                self.nextState.EX["Wrt_reg_addr"] = int(instruction[20:25], 2)

                # Indicate that this is an I-type instruction
                self.nextState.EX["is_I_type"] = 1

                # Read base address from the register file
                self.nextState.EX["Read_data1"] = int(self.myRF.readRF(int(instruction[12:17], 2)), 16)
                self.nextState.EX["Read_data2"] = 0  # Not used in LW

                # Set flags for memory read and write-back enable
                self.nextState.EX["rd_mem"] = 1  # Flag indicating a memory read operation
                self.nextState.EX["wrt_mem"] = 0  # No memory write in LW
                self.nextState.EX["wrt_enable"] = 1  # Enable writing data back to the register

                # Set the ALU operation type for LW
                self.nextState.EX["alu_op"] = 0b00
            
            # Check if it's an SW instruction
            if instruction[25:32] == '0100011':
                # Extract the immediate value (constructed from two parts of the instruction)
                imm = instruction[0:7] + instruction[20:25]
                imm = self.getImmValue(imm)
                self.nextState.EX["Imm"] = imm

                # Extract the base address source register (Rs) and the source data register (Rt)
                self.nextState.EX["Rs"] = int(instruction[12:17], 2)
                self.nextState.EX["Rt"] = int(instruction[7:12], 2)

                # There's no write-back to a register in SW, so Wrt_reg_addr is set to 0
                self.nextState.EX["Wrt_reg_addr"] = 0

                # Indicate that this is not an I-type instruction
                self.nextState.EX["is_I_type"] = 0

                # Read base address and data to be stored from the register file
                self.nextState.EX["Read_data1"] = int(self.myRF.readRF(int(instruction[12:17], 2)), 16)
                self.nextState.EX["Read_data2"] = int(self.myRF.readRF(int(instruction[7:12], 2)), 16)

                # Set flags for memory write (no read operation, no write-back to register)
                self.nextState.EX["rd_mem"] = 0
                self.nextState.EX["wrt_mem"] = 1
                self.nextState.EX["wrt_enable"] = 0

                # Set the ALU operation type for SW
                self.nextState.EX["alu_op"] = 0b00
                
            # Check if it's a Halt instruction
            if instruction[25:32] == '1111111':
                # Setting 'nop' flags in all stages to True, indicating no operation should be performed
                self.nextState.IF["nop"] = True
                self.state.IF["nop"] = True
                self.nextState.ID["nop"] = True
                self.state.ID["nop"] = True
                self.nextState.EX["nop"] = True

                # Check if there is no ongoing read/write operation in the MEM stage
                if self.nextState.MEM["rd_mem"] == 0 and self.nextState.MEM["wrt_mem"] == 0 and self.nextState.MEM["wrt_enable"] == 0:
                    self.nextState.MEM["nop"] = True

                # Check if there is no ongoing write operation in the WB stage
                if self.nextState.WB["wrt_enable"] == 0:
                    self.nextState.WB["nop"] = True

                # Keep the PC the same, effectively halting the pipeline
                self.nextState.IF["PC"] = self.state.IF["PC"]
            # ------------------- Hazard Examination -----------------
            # EX/MEM forwarding
            if self.state.EX["wrt_enable"] == 1:
                # For R-type instructions with no memory read
                if self.state.EX["rd_mem"] == 0:
                    # Forward data from MEM to EX if Rs matches the write-back register and Rs is not zero
                    if self.nextState.EX["Rs"] == self.state.EX["Wrt_reg_addr"] and self.nextState.EX["Rs"] != 0:
                        self.nextState.EX["Read_data1"] = self.nextState.MEM["ALUresult"]

                    # Forward data from MEM to EX if Rt matches the write-back register and Rt is not zero
                    elif self.nextState.EX["Rt"] == self.state.EX["Wrt_reg_addr"] and self.nextState.EX["Rt"] != 0:
                        self.nextState.EX["Read_data2"] = self.nextState.MEM["ALUresult"]

                # Handling load-use data hazard for LW instructions
                elif self.state.EX["rd_mem"] == 1:
                    # Check for hazard condition with Rs or Rt matching the write-back register
                    if (self.nextState.EX["Rs"] == self.state.EX["Wrt_reg_addr"] and self.nextState.EX["Rs"] != 0) \
                        or (self.nextState.EX["Rt"] == self.state.EX["Wrt_reg_addr"] and self.nextState.EX["Rt"] != 0):
                        
                        # Introduce a stall (bubble) in the pipeline
                        self.nextState.EX["nop"] = True  # Stall the EX stage
                        self.nextState.EX["wrt_enable"] = 0 # Clear write enable to avoid re-detection of hazard
                        self.nextState.EX["wrt_mem"] = 0
                        self.nextState.EX["rd_mem"] = 0
                        self.state.IF["PC"] = self.state.IF["PC"] - 4  # Reload the instruction

            # MEM/WB forwarding
            if self.state.MEM["wrt_enable"] == 1:
                # Forward data from WB to EX if Rs matches the write-back register and Rs is not zero
                if self.nextState.EX["Rs"] == self.state.MEM["Wrt_reg_addr"] and self.nextState.EX["Rs"] != 0:
                    self.nextState.EX["Read_data1"] = self.nextState.WB["Wrt_data"]

                # Forward data from WB to EX if Rt matches the write-back register and Rt is not zero
                elif self.nextState.EX["Rt"] == self.state.MEM["Wrt_reg_addr"] and self.nextState.EX["Rt"] != 0:
                    self.nextState.EX["Read_data2"] = self.nextState.WB["Wrt_data"]
            #-------------------- End of Hazard Examination ------------
        
        # --------------------- IF stage ---------------------
        # Proceed if the IF stage is not marked as 'nop' (no operation)
        if self.state.IF["nop"] == False:
            # Read the instruction from memory at the current PC and convert it to a binary string
            self.nextState.ID["Instr"] = bin(int(self.ext_imem.readInstr(self.state.IF.get("PC")), 16))[2:].zfill(32)

            # Check if PC is at the initial state (0)
            if self.nextState.IF["PC"] == 0:
                # Preserve the 'nop' status from the IF to ID stage
                self.nextState.ID["nop"] = self.state.IF["nop"]

                # Update the PC for the next cycle
                if self.state.IF["PC"] < len(self.ext_imem.IMem) - 4 and self.nextState.ID["Instr"][25:32] != '1111111':
                    # Not at the end of instruction memory and not a halt instruction
                    self.endOfFile = False
                    self.nextState.IF["PC"] = self.state.IF["PC"] + 4
                else:
                    # At the end of instruction memory or a halt instruction
                    self.endOfFile = True
                    self.nextState.IF["PC"] = self.state.IF["PC"]
        else: # can only land here from ID.halt 111111
            self.nextState.IF["nop"] = self.state.IF["nop"] # Loop IF stage with True nop
            self.nextState.ID["nop"] = self.state.IF["nop"] # Set next ID to nop too
            self.nextState.IF["PC"] = self.state.IF["PC"]
        #---------------------- End Cycle --------------------
        # Checking for halt condition
        if self.state.IF["nop"] and self.state.ID["nop"] and self.state.EX["nop"] and self.state.MEM["nop"] and self.state.WB["nop"]:
            # If all stages are in 'nop' state, set the halted flag to True
            self.halted = True

        # Output the current state of the Register File (RF) for this cycle
        self.myRF.outputRF(self.cycle)  # Dump RF contents for debugging or analysis

        # Print the state of the CPU after executing the current cycle
        self.printState(self.nextState, self.cycle)  # Useful for understanding the progression of the simulation

        # Update the current state to the next state at the end of the cycle
        # This prepares the simulator for the next cycle of execution
        self.state = self.nextState

        # Create a new state for the next cycle
        self.nextState = State()

        # Increment the cycle counter, moving to the next cycle
        self.cycle += 1

    def printState(self, state, cycle):
        printstate = ["-"*70+"\n", "State after executing cycle: " + str(cycle) + "\n"]
        
        for stage in ['IF', 'ID', 'EX', 'MEM', 'WB']:
            for key, val in getattr(state, stage).items():
                # Jump the EX.alu_control 
                if stage == 'EX' and key == 'alu_control':
                    continue

            
                if key in ['Read_data1', 'Read_data2', 'ALUresult', 'Store_data', 'Wrt_data']:
                    formatted_val = format_binary(val, 32)
                elif key in ['Rs', 'Rt', 'Wrt_reg_addr']:
                    formatted_val = format_binary(val, 5)
                elif key == 'Imm':
                    formatted_val = format_binary(val, 12)  
                elif key == 'alu_op':
                    formatted_val = format_binary(val, 2)
                else:
                    formatted_val = str(val)

                printstate.append(f"{stage}.{key}: {formatted_val}\n")

        if cycle == 0: perm = "w"
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
                rp.writelines("IPC -> "+str((ssCore.numInstructions-1)/ssCore.cycle)+"\n\n")
                rp.writelines("Performance of Five Stage:\n")
                rp.writelines("#Cycles -> "+str(fsCore.cycle)+"\n")
                rp.writelines("#Instructions -> "+str(ssCore.numInstructions-1)+"\n")
                rp.writelines("CPI -> "+str(fsCore.cycle/(ssCore.numInstructions-1))+"\n")
                rp.writelines("IPC -> "+str((ssCore.numInstructions-1)/fsCore.cycle)+"\n\n")
            break
    
    # dump SS and FS data mem.
    dmem_ss.outputDataMem()
    dmem_fs.outputDataMem()