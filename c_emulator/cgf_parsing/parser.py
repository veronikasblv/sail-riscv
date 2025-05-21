from sys import argv
import riscv_isac.coverage as cov
from riscv_isac.isac import preprocessing
from riscv_isac.cgf_normalize import expand_cgf
from riscv_isac.plugins.translator_cgf import Translate_cgf

input_files = argv[1:]
f = open("./c_emulator/cgf_parsing/middle",'w')

cgf = Translate_cgf(expand_cgf(input_files,32,64))


def opcode_def(inst_name):
    inst_name = inst_name.replace(".","_")
    for inst in RISCV_INSTS:
        if ("_" + inst_name + "(") in inst:
            return inst


def reg_symbolic(inst, num):
    init_str = ""
    if "rd" in inst:
        init_str += f"      int rd_{num};\n"
        init_str += '      klee_make_symbolic(&rd_{0}, sizeof(int), "rd_{0}");\n'.format(num)
        init_str += '      klee_assume(1 <= rd_{0} && rd_{0} <= 31);\n'.format(num)
    if "rs1" in inst:
        init_str += f"      int rs1_{num};\n      uint64_t rs1_val_{num};\n"
        init_str += '      klee_make_symbolic(&rs1_{0}, sizeof(int), "rs1_{0}");\n'.format(num)
        init_str += '      klee_assume(0 <= rs1_{0} && rs1_{0} <= 31);\n'.format(num)
        init_str += '      klee_make_symbolic(&rs1_val_{0}, sizeof(uint64_t), "rs1_val_{0}");\n'.format(num)
        init_str += '      klee_assume(rs1_val_{0} == 0 && rs1_{0} == 0 || rs1_{0} != 0);\n'.format(num)
    if "rs2" in inst:
        init_str += f"      int rs2_{num};\n      uint64_t rs2_val_{num};\n"
        init_str += '      klee_make_symbolic(&rs2_{0}, sizeof(int), "rs2_{0}");\n'.format(num)
        init_str += '      klee_assume(0 <= rs2_{0} && rs2_{0} <= 31);\n'.format(num)
        init_str += '      klee_make_symbolic(&rs2_val_{0}, sizeof(uint64_t), "rs2_val_{0}");\n'.format(num)
        init_str += '      klee_assume(rs2_val_{0} == 0 && rs2_{0} == 0 || rs2_{0} != 0);\n'.format(num)
    if "imm" in inst:
        init_str += f"      int imm_val_{num};\n"
        init_str += '      klee_make_symbolic(&imm_val_{0}, sizeof(int), "imm_val_{0}");\n'.format(num)
    return init_str


def reg_conditions(reg_str, num, all_reg_consts):
    reg_conds = []
    for reg in all_reg_consts:
        reg_conds.append(f"{reg_str}_{num} == {reg[1:]}")
    return " || ".join(reg_conds)


def comb_conditions(all_combs):
    comb_conds = []
    for comb in all_combs:
        comb_conds.append(comb.replace("and", "&&").replace("or", "||").replace("//", "/").replace("math.pow", "(int)pow"))
    return " || ".join(comb_conds)


def reg_init(inst, num, size):
    buf_str = ""
    if "rs1" in inst:
        buf_str += f"      inst_buf[{size}] = riscv_lui(rs1_{num}, (rs1_val_{num} + 0x800) >> 12);\n"
        buf_str += f"      inst_buf[{size + 1}] = riscv_addi(rs1_{num}, rs1_{num}, rs1_val_{num} & 0xfff);\n"
        size += 2
    if "rs2" in inst:
        buf_str += f"      inst_buf[{size}] = riscv_lui(rs2_{num}, (rs2_val_{num} + 0x800) >> 12);\n"
        buf_str += f"      inst_buf[{size + 1}] = riscv_addi(rs2_{num}, rs2_{num}, rs2_val_{num} & 0xfff);\n"
        size += 2   
    return buf_str, size


def csr_regs(csr_str):
    regs_list = []
    for reg in CSR_REGS:
        if reg in csr_str:
            regs_list.append(reg)
    return regs_list


def assgn_cond_constraints(assgns, conds, num):
    consts = ""
    if conds.find("?") == -1:
        conds = conds.replace("and", "&&").replace("or", "||").replace("//", "/").replace("math.pow", "(uint32_t)pow")
        conds = conds.replace("rs1", f"rs1_{num}").replace("rs2", f"rs2_{num}").replace("rd", f"rd_{num}")
        consts += f"      klee_assume({conds});\n"

    if assgns != "?":
        assgn_list = assgns.split(';')
        for assgn in assgn_list:
            assgn = assgn.replace(f"rs1", f"rs1_{num}").replace("rs2", f"rs2_{num}").replace("rd", f"rd_{num}")
            consts += f"      int {assgn};\n"
    return consts

def instrs_halt(size, set_lim):
    if set_lim:
        return f"      insn_limit = {size + 5};\n      break;\n    {'}'}"
    halt_str = f"      inst_buf[{size}] = riscv_addi(1, 0, 0x1);\n"
    halt_str += f"      inst_buf[{size+1}] = riscv_lui(7, 0x80001);\n"
    halt_str += f"      inst_buf[{size+2}] = riscv_sw(1, 7, 0);\n"
    halt_str += f"      inst_buf[{size+3}] = riscv_jal(0, -8);\n"
    halt_str += f"      break;\n    {'}'}"
    return halt_str


RISCV_INSTS = ["riscv_lui(rd_{0}, imm_val_{0})", "riscv_auipc(rd_{0}, imm_val_{0})", "riscv_jal(rd_{0}, imm_val_{0})", "riscv_jalr(rd_{0}, rs1_{0}, imm_val_{0})", "riscv_beq(rs1_{0}, rs2_{0}, imm_val_{0})", "riscv_bne(rs1_{0}, rs2_{0}, imm_val_{0})", "riscv_blt(rs1_{0}, rs2_{0}, imm_val_{0})", "riscv_bge(rs1_{0}, rs2_{0}, imm_val_{0})", "riscv_bltu(rs1_{0}, rs2_{0}, imm_val_{0})", "riscv_bgeu(rs1_{0}, rs2_{0}, imm_val_{0})", "riscv_lb(rd_{0}, rs1_{0}, imm_val_{0})", "riscv_lh(rd_{0}, rs1_{0}, imm_val_{0})", "riscv_lw(rd_{0}, rs1_{0}, imm_val_{0})", "riscv_lbu(rd_{0}, rs1_{0}, imm_val_{0})", "riscv_lhu(rd_{0}, rs1_{0}, imm_val_{0})", "riscv_sb(rs2_{0}, rs1_{0}, imm_val_{0})", "riscv_sh(rs2_{0}, rs1_{0}, imm_val_{0})", "riscv_sw(rs2_{0}, rs1_{0}, imm_val_{0})", "riscv_addi(rd_{0}, rs1_{0}, imm_val_{0})", "riscv_slti(rd_{0}, rs1_{0}, imm_val_{0})", "riscv_sltiu(rd_{0}, rs1_{0}, imm_val_{0})", "riscv_xori(rd_{0}, rs1_{0}, imm_val_{0})", "riscv_ori(rd_{0}, rs1_{0}, imm_val_{0})", "riscv_andi(rd_{0}, rs1_{0}, imm_val_{0})", "riscv_add(rd_{0}, rs1_{0}, rs2_{0})", "riscv_sub(rd_{0}, rs1_{0}, rs2_{0})", "riscv_sll(rd_{0}, rs1_{0}, rs2_{0})", "riscv_slt(rd_{0}, rs1_{0}, rs2_{0})", "riscv_sltu(rd_{0}, rs1_{0}, rs2_{0})", "riscv_xor(rd_{0}, rs1_{0}, rs2_{0})", "riscv_srl(rd_{0}, rs1_{0}, rs2_{0})", "riscv_sra(rd_{0}, rs1_{0}, rs2_{0})", "riscv_or(rd_{0}, rs1_{0}, rs2_{0})", "riscv_and(rd_{0}, rs1_{0}, rs2_{0})", "riscv_fence_i()", "riscv_ecall()", "riscv_ebreak()"]
ALL_REGS = ["imm", "rd", "rs1", "rs2"]
CSR_REGS = ['mvendorid', 'marchid', 'mimpid', 'mhartid', 'mstatus', 'misa', 'medeleg', 'mideleg', 'mie', 'mtvec', 'mcounteren', 'mscratch', 'mepc', 'mcause', 'mtval', 'mip', 'pmpcfg0', 'pmpcfg1', 'pmpcfg2', 'pmpcfg3', 'mcycle', 'minstret', 'mcycleh', 'minstreth', 'mcountinhibit', 'tselect', 'tdata1', 'tdata2', 'tdata3', 'dcsr', 'dpc', 'dscratch0', 'dscratch1', 'sstatus', 'sedeleg', 'sideleg', 'sie', 'stvec', 'scounteren', 'sscratch', 'sepc', 'scause', 'stval', 'sip', 'satp', 'vxsat', 'fflags', 'frm', 'fcsr']

case_num = 0
print("  int xlen = 32, case_num;", file=f)
print('  klee_make_symbolic(&case_num, sizeof(int), "case_num");', file=f)
print("  switch (case_num) {", file=f)
for cov_label,value in cgf.items():
    if cov_label != 'datasets':
        if 'mnemonics' in value:     
            mnemonics = list(value['mnemonics'].keys())[0]
            instr = opcode_def(mnemonics)
            if instr == None:
                continue
            
            print(f"    case {case_num}: {'{'}", file=f)
            print(reg_symbolic(instr, 0), end="", file=f)
            
            if 'rs1' in value and len(value['rs1']) != 0:
                print(f"      klee_assume({ reg_conditions('rs1', 0, value['rs1'].keys()) });", file=f)

            if 'rs2' in value and len(value['rs2']) != 0:
                print(f"      klee_assume({ reg_conditions('rs2', 0, value['rs2'].keys()) });", file=f)

            if 'rd' in value and len(value['rd']) != 0:
                print(f"      klee_assume({ reg_conditions('rd', 0, value['rd'].keys()) });", file=f)

            if 'op_comb' in value and len(value['op_comb']) != 0 :
                op_conditions = comb_conditions(value['op_comb'].keys())
                op_conditions = op_conditions.replace("rs1", "rs1_0").replace("rs2", "rs2_0").replace("rd", "rd_0")
                print(f"      klee_assume({op_conditions});", file=f)

            if 'val_comb' in value and len(value['val_comb']) != 0:
                val_conditions = comb_conditions(value['val_comb'].keys())
                val_conditions = val_conditions.replace("val", "val_0")
                print(f"      klee_assume({val_conditions});", file=f)

            inst_buf_str, buf_len = reg_init(instr, 0, 0)
            print(inst_buf_str, file=f)
            print(f"      inst_buf[{buf_len}] = {instr.format(0)};\n", file=f)
            buf_len += 1
            print(instrs_halt(buf_len, True), file=f)
            case_num += 1                

        if "cross_comb" in value:
            for cross_comb in value["cross_comb"].keys():
                data = cross_comb.split('::')
                inst_lst = data[0].replace(' ', '')[1:-1].split(':')
                assgn_lst = data[1].replace(' ', '')[1:-1].split(':')
                cond_lst = data[2].lstrip().rstrip()[1:-1].split(':')

                print(f"    case {case_num}: {'{'}", file=f)
                buf_len = 0
                for instr_num in range(len(inst_lst)):
                    instr_str = inst_lst[instr_num]
                    assgn_str = assgn_lst[instr_num]
                    cond_str = cond_lst[instr_num]
                    if instr_str != "?" and instr_str.find("(") == -1:
                        instr = opcode_def(instr_str)
                        if instr == None:
                            continue
                        
                        print(f"      uint32_t instr_{instr_num};", file=f)
                        print('      klee_make_symbolic(&instr_{0}, sizeof(uint32_t), "instr_{0}");'.format(instr_num), file=f)
                        print(reg_symbolic(instr, instr_num), end="", file=f)

                        print(assgn_cond_constraints(assgn_str, cond_str, instr_num), end="", file=f)

                        print(f"      klee_assume(instr_{instr_num} == ({instr.format(instr_num)}));", file=f)
                        inst_buf_str, buf_len = reg_init(instr, instr_num, buf_len)
                        print(inst_buf_str, file=f)
                    else:
                        print(f"      uint32_t instr_{instr_num};", file=f)
                        print('      klee_make_symbolic(&instr_{0}, sizeof(uint32_t), "instr_{0}");'.format(instr_num), file=f)
                        print(reg_symbolic(ALL_REGS, instr_num), end="", file=f)
                        
                        print(assgn_cond_constraints(assgn_str, cond_str, instr_num), end="", file=f)
                        
                        regs = []
                        if "rs1" in assgn_str or "rs1" in cond_str:
                            regs += "rs1"
                        if "rs2" in assgn_str or "rs2" in cond_str:
                            regs += "rs2"
                        if "rd" in assgn_str or "rd" in cond_str:
                            regs += "rd"

                        instr_conditions = []
                        if instr_str.find("(") != -1:
                            insts_list = instr_str[1:-1].split(',')
                            for instr_name in insts_list:
                                instr = opcode_def(instr_name)
                                if instr == None:
                                    continue
                                instr_conditions.append(f" instr_{instr_num} == ({instr.format(instr_num)})")    
                        else:
                            for instr in RISCV_INSTS:
                                if all(reg in instr for reg in regs):
                                    instr_conditions.append(f" instr_{instr_num} == ({instr.format(instr_num)})")

                        print("      klee_assume(", " || ".join(instr_conditions), ");", file=f)
                        inst_buf_str, buf_len = reg_init(ALL_REGS, instr_num, buf_len)
                        print(inst_buf_str, file=f)

                for instr_num in range(len(inst_lst)):
                    print(f"      inst_buf[{buf_len}] = instr_{instr_num};", file=f)
                    buf_len += 1

                print(instrs_halt(buf_len, True), file=f)
                case_num += 1  
print("    default:\n      exit(0);\n  }", file=f)
