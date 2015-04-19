package Translate;
import Symbol.Symbol;
import Tree.BINOP;
import Tree.CJUMP;
import Temp.Temp;
import Temp.Label;

public class Translate {
	
  public Frame.Frame frame;
  
  public Translate(Frame.Frame f) {
    frame = f;
  }
  
  private Frag frags;
  
  public void procEntryExit(Level level, Exp body) {
    Frame.Frame myframe = level.frame;
    Tree.Exp bodyExp = body.unEx();
    Tree.Stm bodyStm;
    if (bodyExp != null)
      bodyStm = MOVE(TEMP(myframe.RV()), bodyExp);
    else
      bodyStm = body.unNx();
    ProcFrag frag = new ProcFrag(myframe.procEntryExit1(bodyStm), myframe);
    frag.next = frags;
    frags = frag;
  }
  
  public Frag getResult() {
    return frags;
  }

  private static Tree.Exp CONST(int value) {
    return new Tree.CONST(value);
  }
  
  private static Tree.Exp NAME(Label label) {
    return new Tree.NAME(label);
  }
  
  private static Tree.Exp TEMP(Temp temp) {
    return new Tree.TEMP(temp);
  }
  
  private static Tree.Exp BINOP(int binop, Tree.Exp left, Tree.Exp right) {
    return new Tree.BINOP(binop, left, right);
  }
  
  private static Tree.Exp MEM(Tree.Exp exp) {
    return new Tree.MEM(exp);
  }
  
  private static Tree.Exp CALL(Tree.Exp func, Tree.ExpList args) {
    return new Tree.CALL(func, args);
  }
  
  private static Tree.Exp ESEQ(Tree.Stm stm, Tree.Exp exp) {
    if (stm == null)
      return exp;
    return new Tree.ESEQ(stm, exp);
  }

  private static Tree.Stm MOVE(Tree.Exp dst, Tree.Exp src) {
    return new Tree.MOVE(dst, src);
  }
  
  private static Tree.Stm UEXP(Tree.Exp exp) {
    return new Tree.UEXP(exp);
  }
  
  private static Tree.Stm JUMP(Label target) {
    return new Tree.JUMP(target);
  }
  
  private static Tree.Stm CJUMP(int relop, Tree.Exp l, Tree.Exp r, Label t, Label f) {
    return new Tree.CJUMP(relop, l, r, t, f);
  }
  
  private static Tree.Stm SEQ(Tree.Stm left, Tree.Stm right) {
    if (left == null)
      return right;
    if (right == null)
      return left;
    return new Tree.SEQ(left, right);
  }
  
  private static Tree.Stm LABEL(Label label) {
    return new Tree.LABEL(label);
  }

  private static Tree.ExpList ExpList(Tree.Exp head, Tree.ExpList tail) {
    return new Tree.ExpList(head, tail);
  }
  
  private static Tree.ExpList ExpList(Tree.Exp head) {
    return ExpList(head, null);
  }
  
  private static Tree.ExpList ExpList(ExpList exp) {
    if (exp == null)
      return null;
    return ExpList(exp.head.unEx(), ExpList(exp.tail));
  }

  public Exp Error() {
	System.out.println("Error"); 
    return new Ex(CONST(0));
  }

  public Exp SimpleVar(Access access, Level level) { 
	  Tree.Exp fp = TEMP(access.home.frame.FP());
	  
	  Level l = level;
	  while (l != access.home) {
	     fp = level.frame.formals.head.exp(fp);
	     l = level.parent;
	  }
	  return new Ex(access.acc.exp(fp));
  }

  public Exp FieldVar(Exp record, int index) {
	  System.out.println("FieldVar"); 
    return Error();
  }

  public Exp SubscriptVar(Exp array, Exp index) {
	  Temp t3 = new Temp();
	  Temp t4 = new Temp();
	  Label badSub = frame.badSub();
	  Label l0 = new Label();
	  Label l1 = new Label();
	  int wSize = frame.wordSize();
	  return new Ex(ESEQ(
	    				SEQ(
    						MOVE(TEMP(t3), array.unEx()), 
    						SEQ(
								MOVE(TEMP(t4), index.unEx()), 
								SEQ(
									CJUMP(2, TEMP(t4), CONST(0), badSub, l0), 
									SEQ(
										LABEL(l0), 
										SEQ(
												CJUMP(3, TEMP(t4), MEM(BINOP(0, TEMP(t3), CONST(-wSize))), badSub, l1), 
												LABEL(l1)))))), 
						MEM(
							BINOP(Absyn.OpExp.PLUS, TEMP(t3), 
							BINOP(Absyn.OpExp.MUL, TEMP(t4), CONST(wSize))))));
  }

  public Exp NilExp() {
	  return new Nx(null);
  }

  public Exp IntExp(int value) {
    return new Ex(CONST(value));
  }

  private java.util.Hashtable strings = new java.util.Hashtable();
  
  public Exp StringExp(String lit) {
    String u = lit.intern();
    Label lab = (Label)strings.get(u);
    if (lab == null) {
      lab = new Label();
      strings.put(u, lab);
      DataFrag frag = new DataFrag(frame.string(lab, u));
      frag.next = frags;
      frags = frag;
    }
    return new Ex(NAME(lab));
  }

  private Tree.Exp CallExp(Symbol f, ExpList args, Level from) {
    return frame.externalCall(f.toString(), ExpList(args));
  }
  
  private Tree.Exp CallExp(Level f, ExpList args, Level from) {
    throw new Error("Translate.CallExp unimplemented");
  }

  public Exp FunExp(Symbol f, ExpList args, Level from) {
    return new Ex(CallExp(f, args, from));
  }
  
  public Exp FunExp(Level f, ExpList args, Level from) {
    return new Ex(CallExp(f, args, from));
  }
  
  public Exp ProcExp(Symbol f, ExpList args, Level from) {
    return new Nx(UEXP(CallExp(f, args, from)));
  }
  
  public Exp ProcExp(Level f, ExpList args, Level from) {
    return new Nx(UEXP(CallExp(f, args, from)));
  }

  public Exp OpExp(int op, Exp left, Exp right) {
  	  switch(op){
	  case Absyn.OpExp.PLUS:
		  return new Ex(BINOP(op, left.unEx(), right.unEx()));
	  case Absyn.OpExp.MINUS:
		  return new Ex(BINOP(op, left.unEx(), right.unEx()));
	  case Absyn.OpExp.MUL:
		  return new Ex(BINOP(op, left.unEx(), right.unEx()));
	  case Absyn.OpExp.DIV:
		  return new Ex(BINOP(op, left.unEx(), right.unEx()));
	  case Absyn.OpExp.EQ:
		  return new RelCx(CJUMP.EQ, left.unEx(), right.unEx());
	  case Absyn.OpExp.NE:
		  return new RelCx(CJUMP.NE, left.unEx(), right.unEx());
	  case Absyn.OpExp.LT:
		  return new RelCx(CJUMP.LT, left.unEx(), right.unEx());
	  case Absyn.OpExp.LE:
		  return new RelCx(CJUMP.LE, left.unEx(), right.unEx());
	  case Absyn.OpExp.GT:
		  return new RelCx(CJUMP.GT, left.unEx(), right.unEx());
	  case Absyn.OpExp.GE:
		  return new RelCx(CJUMP.GE, left.unEx(), right.unEx());
	  default:
		  return Error();
	  }
  }

  public Exp StrOpExp(int op, Exp left, Exp right) {
  	  switch(op){
	  case Absyn.OpExp.PLUS:
		  return new Ex(BINOP(op, left.unEx(), right.unEx()));
	  case Absyn.OpExp.MINUS:
		  return new Ex(BINOP(op, left.unEx(), right.unEx()));
	  case Absyn.OpExp.MUL:
		  return new Ex(BINOP(op, left.unEx(), right.unEx()));
	  case Absyn.OpExp.DIV:
		  return new Ex(BINOP(op, left.unEx(), right.unEx()));
	  case Absyn.OpExp.EQ:
		  return new RelCx(CJUMP.EQ, left.unEx(), right.unEx());
	  case Absyn.OpExp.NE:
		  return new RelCx(CJUMP.NE, left.unEx(), right.unEx());
	  case Absyn.OpExp.LT:
		  return new RelCx(CJUMP.LT, left.unEx(), right.unEx());
	  case Absyn.OpExp.LE:
		  return new RelCx(CJUMP.LE, left.unEx(), right.unEx());
	  case Absyn.OpExp.GT:
		  return new RelCx(CJUMP.GT, left.unEx(), right.unEx());
	  case Absyn.OpExp.GE:
		  return new RelCx(CJUMP.GE, left.unEx(), right.unEx());
	  default:
		  return Error();
  	  }
  }

  public Exp RecordExp(ExpList init) {
	  System.out.println("RecordExp"); 
    return Error();
  }

  public Exp SeqExp(ExpList e) {
	  if(e == null){
		  return NilExp();
	  }
	  if(e.head == null){
		  return NilExp();
	  }
	  if(e.tail == null){ 
		  return new Ex(e.head.unEx());
	  }
	  return new Ex(ESEQ(e.head.unNx(), SeqExp(e.tail).unEx()));
  }
  

  public Exp AssignExp(Exp lhs, Exp rhs) {
	  // That simple?
    return new Nx(MOVE(lhs.unEx(), rhs.unEx()));
  }

  public Exp IfExp(Exp cc, Exp aa, Exp bb) {

	  if (!(aa instanceof Nx))
	  	  return new IfThenElseExp(cc, aa, bb);
	  			  
	  Label l1 = new Label();
	  Label l2 = new Label();
	  return new Nx(
		  SEQ(
			  SEQ(
				  cc.unCx(l1,l2), 
				  SEQ(
				  	SEQ(
			  			LABEL(l1), 
			  			bb.unNx()
		  			), 
				  	JUMP(l2)
			  	)
			  ), 
			  LABEL(l2)
		  )
	  );
  }

  public Exp WhileExp(Exp test, Exp body, Label done) {
	  Label l0 = done; 
	  Label l1 = new Label();
	  Label l2 = new Label();
	  return new Nx(SEQ(
			  			SEQ(
		  					SEQ(
	  							LABEL(l1), 
	  							test.unCx(l2, l0)), 
  							SEQ(
								SEQ(
									LABEL(l2), 
									body.unNx()), 
								JUMP(l1))),
						LABEL(l0)));

  }

  public Exp ForExp(Access i, Exp lo, Exp hi, Exp body, Label done) {
	Temp low = i.home.frame.FP();
	Temp high = new Temp(); 
	Label block = new Label();
	Label increment = new Label(); 
	
	return new Nx(SEQ(
					  SEQ(
						  SEQ(
							  SEQ(
								  MOVE(
									  i.acc.exp(TEMP(low)),
									  lo.unEx()),
								  MOVE(TEMP(high), hi.unEx())),
							  CJUMP(CJUMP.LE, i.acc.exp(TEMP(low)), TEMP(high), block, done)),
						  SEQ(
							  SEQ(
								  SEQ(
									  LABEL(block),
									  body.unNx()),
								  CJUMP(CJUMP.LT, i.acc.exp(TEMP(low)), TEMP(high), increment, done)),
							  SEQ(
								  SEQ(
									  LABEL(increment),
									  MOVE(i.acc.exp(TEMP(low)), BINOP(Absyn.OpExp.PLUS, i.acc.exp(TEMP(low)), CONST(1)))),
					     JUMP(block)))),
					  LABEL(done)));
	}

  public Exp BreakExp(Label done) {
	  return new Nx(JUMP(done));
  }

  public Exp LetExp(ExpList lets, Exp body) {
	  if(lets == null && body == null){
		  return NilExp();
	  }
	  Tree.Exp b = body.unEx();
	  if( b == null){
		  return new Nx(SEQ(decList(lets), body.unNx()));
	  }
	  return new Ex(ESEQ(decList(lets), body.unEx()));
  }
  
  private Tree.Stm decList(ExpList l){
	  if(l == null){
		  return NilExp().unNx();
	  }
	  if(l.head == null){
		  return null;
	  }
	  if(l.tail == null){
		  return l.head.unNx(); 
	  }
	  return SEQ(l.head.unNx(), decList(l.tail));
  }


  public Exp ArrayExp(Exp size, Exp init) {
	  // makes call to something
	  return new Ex(frame.externalCall("initArray", ExpList(size.unEx(), ExpList(init.unEx()))));  
  }

  public Exp VarDec(Access a, Exp init) {
	  Tree.Exp fp = TEMP(a.home.frame.FP());
	  return new Nx(MOVE(a.acc.exp(fp), init.unEx()));
  }

  public Exp TypeDec() {
    return new Nx(null);
  }

  public Exp FunctionDec() {
    return new Nx(null);
  }
}
