#include <stdio.h>
#include <stdlib.h>

#include <ir/ir.h>
#include <target/util.h>

static char* TLHSREG[] = {
    "A",
    "B",
    "C",
    "D",
    "BP",
    "SP",
};

static int tlhs_inst_pc = 0;

static void tlhs_emit_line_2op(const char* imm, Inst* inst) {
    if (inst->src.type == REG) {
        emit_line("Run total_cnt debug %6d cpu = Run_ (total_cnt + 1) debug (%s_REG cpu %s %s)",
                tlhs_inst_pc, imm, TLHSREG[inst->dst.reg], TLHSREG[inst->src.reg]);
    } else {
        emit_line("Run total_cnt debug %6d cpu = Run_ (total_cnt + 1) debug (%s_IMM cpu %s %d)",
                tlhs_inst_pc, imm, TLHSREG[inst->dst.reg], inst->src.imm);
    }
}

static void tlhs_emit_line_conditional_jump(const char* imm, Inst* inst) {
    if (inst->jmp.type == REG && inst->src.type == REG) {
      emit_line("Run total_cnt debug %6d cpu = Run_ (total_cnt + 1) debug (%s_REG_REG cpu %d %s %s)",
              tlhs_inst_pc, imm, inst->jmp.reg, TLHSREG[inst->dst.reg], TLHSREG[inst->src.reg]);
    } else if (inst->jmp.type == REG && inst->src.type != REG){
      emit_line("Run total_cnt debug %6d cpu = Run_ (total_cnt + 1) debug (%s_REG_IMM cpu %d %s %s)",
              tlhs_inst_pc, imm, inst->jmp.reg, TLHSREG[inst->dst.reg], inst->src.imm);
    } else if (inst->jmp.type != REG && inst->src.type == REG){
      emit_line("Run total_cnt debug %6d cpu = Run_ (total_cnt + 1) debug (%s_IMM_REG cpu %d %s %d)",
              tlhs_inst_pc, imm, inst->jmp.imm, TLHSREG[inst->dst.reg], TLHSREG[inst->src.reg]);
    } else {
      emit_line("Run total_cnt debug %6d cpu = Run_ (total_cnt + 1) debug (%s_IMM_IMM cpu %d %s %d)",
              tlhs_inst_pc, imm, inst->jmp.imm, TLHSREG[inst->dst.reg], inst->src.imm);
    }
}

static void tlhs_emit_inst(Inst* inst) {
  switch (inst->op) {
  case MOV:
    tlhs_emit_line_2op("MV", inst);
    break;

  case ADD:
    tlhs_emit_line_2op("ADD", inst);
    break;

  case SUB:
    tlhs_emit_line_2op("SUB", inst);
    break;

  case LOAD:
    tlhs_emit_line_2op("LOAD", inst);
    break;

  case STORE:
    tlhs_emit_line_2op("STORE", inst);
    break;

  case PUTC:
    if (inst->src.type == REG) {
        emit_line("Run total_cnt debug %6d cpu = Run_ (total_cnt + 1) debug (PUTC_REG cpu %s)", tlhs_inst_pc, TLHSREG[inst->src.reg]);
    } else {
        emit_line("Run total_cnt debug %6d cpu = Run_ (total_cnt + 1) debug (PUTC_IMM cpu %d)", tlhs_inst_pc, inst->src.imm);
    }
    break;

  case GETC:
    emit_line("Run total_cnt debug %6d cpu = Run_ (total_cnt + 1) debug (GETC cpu %s)", tlhs_inst_pc, TLHSREG[inst->dst.reg]);
    break;

  case EXIT:
    emit_line("Run total_cnt debug %6d cpu = cpu", tlhs_inst_pc);
    break;

  case DUMP:
    emit_line("Run total_cnt debug %6d cpu = Run_ (total_cnt + 1) debug (NOP cpu) -- FIXME: DUMP", tlhs_inst_pc);
    break;

  case EQ:
    tlhs_emit_line_2op("EQ", inst);
    break;

  case NE:
    tlhs_emit_line_2op("NE", inst);
    break;

  case LT:
    tlhs_emit_line_2op("LT", inst);
    break;

  case GT:
    tlhs_emit_line_2op("GT", inst);
    break;

  case LE:
    tlhs_emit_line_2op("LE", inst);
    break;

  case GE:
    tlhs_emit_line_2op("GE", inst);
    break;

  case JEQ:
    tlhs_emit_line_conditional_jump("JEQ", inst);
    break;

  case JNE:
    tlhs_emit_line_conditional_jump("JNE", inst);
    break;

  case JLT:
    tlhs_emit_line_conditional_jump("JLT", inst);
    break;

  case JGT:
    tlhs_emit_line_conditional_jump("JGT", inst);
    break;

  case JLE:
    tlhs_emit_line_conditional_jump("JLE", inst);
    break;

  case JGE:
    tlhs_emit_line_conditional_jump("JGE", inst);
    break;

  case JMP:
    if (inst->jmp.type == REG) {
        emit_line("Run total_cnt debug %6d cpu = Run_ (total_cnt + 1) debug (JUMP_REG cpu %s)", tlhs_inst_pc, TLHSREG[inst->jmp.reg]);
    } else {
        emit_line("Run total_cnt debug %6d cpu = Run_ (total_cnt + 1) debug (JUMP_IMM cpu %d)", tlhs_inst_pc, inst->jmp.imm);
    }
    break;

  default:
    error("oops");
  }
  tlhs_inst_pc += 1;
}

void target_tlhs(Module* module) {
    // memory initialize
    emit_line("type family Init (pc :: Nat) (mem :: Mem) where");
    inc_indent();
    Data* data = module->data;
    for (int mp=0; data; data=data->next, mp++) {
        emit_line("Init %6d mem = Init %d (Store mem %d %d)", mp, mp+1, mp, data->v);
    }
    emit_line("Init _ mem = mem");
    dec_indent();
    emit_line("");

    int pc_cnt = 0;
    for (Inst* inst = module->text; inst; inst = inst->next) {
        pc_cnt++;
    }

    // PC2ADDR: absorb difference of elvm and tlhs
    emit_line("type family PC2ADDR (pc :: Nat) where");
    inc_indent();
    int prev_pc = -1;
    int tlhs_pc = 0;
    for (Inst* inst = module->text; inst; inst = inst->next, tlhs_pc++) {
        if (prev_pc != inst->pc) {
            emit_line("PC2ADDR %6d = %d", inst->pc, tlhs_pc);
        }
        prev_pc = inst->pc;
    }
    dec_indent();
    emit_line("");

    // inst
    emit_line("type family Run (total_cnt :: Nat) (debug :: [Nat]) (pc :: Nat) cpu where");
    inc_indent();
    for (Inst* inst = module->text; inst; inst = inst->next) {
        tlhs_emit_inst(inst);
    }
    dec_indent();
}
