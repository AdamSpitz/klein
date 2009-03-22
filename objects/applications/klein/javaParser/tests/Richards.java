//  Based on original version written in BCPL by Dr Martin Richards
//  in 1981 at Cambridge University Computer Laboratory, England
//  and a C++ version derived from a Smalltalk version written by
//  L Peter Deutsch.
//  Java version:  Copyright (C) 1995 Sun Microsystems, Inc.
//  Translation from C++, Mario Wolczko
//  Outer loop added by Alex Jacoby

package COM.sun.labs.kanban.richards_deutsch_acc_virtual;

import Benchmark;

//----- Packet -------------------------------------------------------

class Packet {
  static final int BUFSIZE = 4;

  private Packet link;
  private int id;
  private int kind;
  private int datum;
  private int[] data = new int[BUFSIZE];

  Packet(Packet l, int i, int k) {
    SetLink(l);
    SetIdent(i);
    SetKind(k);
    SetDatum(0);
    for(int j = 0; j < BUFSIZE; j++)
      SetData(j, 0);
  }

  Packet Link() { return link; }
  int 	Ident() { return id; }
  int    Kind() { return kind; }
  int   Datum() { return datum; }

  void SetLink(Packet l)     { link = l; }
  void SetIdent(int i)       { id = i; }
  void SetKind(int k)        { kind = k; }
  void SetDatum(int d)       { datum = d; }

  int     Data(int i)        { return data[i]; }
  void SetData(int i, int d) { data[i] = d; }

  Packet append_to(Packet list) {
    SetLink(null);
    if (list == null) 
      return this;
    else {
      Packet p = list;
      Packet next = p.Link();
      while (next != null) {
        p = next;
	next = p.Link();
      }
      p.SetLink(this);
      return list;
    }
  }

}

//----- Task Records------------------------------

abstract class TaskRec { } // so we have a common type for all task records

class DeviceTaskRec extends TaskRec {
  private Packet pending;

  DeviceTaskRec()           { pending = null; }
  Packet Pending()          { return pending; }
  void SetPending(Packet p) { pending = p; }
}


class IdleTaskRec extends TaskRec {
  private int control, count;

  IdleTaskRec() { control = 1; count = 10000; }
  int Control() { return control; }
  int Count()   { return count; }
  void SetControl(int n) { control = n; }
  void SetCount(int n)   { count = n; }
}


class HandlerTaskRec extends TaskRec {
  private Packet workIn, deviceIn;

  HandlerTaskRec() { workIn = deviceIn = null; }

  Packet   WorkIn() { return workIn; }
  Packet DeviceIn() { return deviceIn; }

  void SetDeviceIn(Packet p) { deviceIn = p; }
  void SetWorkIn  (Packet p) { workIn = p; }

  Packet   WorkInAdd(Packet p) { return workIn = p.append_to(workIn); }
  Packet DeviceInAdd(Packet p) { return deviceIn = p.append_to(deviceIn); }
}



class WorkerTaskRec extends TaskRec {
  private int destination;
  private int count;

  WorkerTaskRec() { destination = Richards.I_HANDLERA; count = 0; }

  int       Count() { return count; }
  int Destination() { return destination; }

  void SetCount      (int n) { count = n; }
  void SetDestination(int d) { destination = d; }
}


//----- Task ---------------------------------------------------------

class TaskState {

  protected boolean packetPending, taskWaiting, taskHolding;

  TaskState() {
    packetPending = true;
    taskWaiting = false;
    taskHolding = false;
  }

  TaskState PacketPending() {
    packetPending = true;
    taskWaiting = taskHolding = false;
    return this;
  }
  TaskState Waiting() {
    packetPending = taskHolding = false;
    taskWaiting = true;
    return this;
  }
  TaskState Running() {
    packetPending = taskWaiting = taskHolding = false;
    return this;
  }
  TaskState WaitingWithPacket() {
    packetPending = taskWaiting = true; taskHolding = false;
    return this;
  }

  /* accessing */
  boolean IsPacketPending() { return packetPending; }
  boolean IsTaskWaiting()   { return taskWaiting; }
  boolean IsTaskHolding()   { return taskHolding; }

  void SetTaskHolding(boolean state) { taskHolding = state; }
  void SetTaskWaiting(boolean state) { taskWaiting = state; }
  void SetPacketPending(boolean state) { packetPending = state; }

  /* testing */ 
  boolean IsTaskHoldingOrWaiting() {
    return IsTaskHolding() || !IsPacketPending() && IsTaskWaiting();
  }
  boolean IsWaitingWithPacket() {
    return IsPacketPending() && IsTaskWaiting() && !IsTaskHolding();
  }
}

abstract class Task extends TaskState {

  static int layout = 0;

  protected Task link;
  protected int id;
  protected int pri;
  protected Packet wkq;
  protected TaskRec handle;

  Task      Link() { return link; }
  int      Ident() { return id; }
  int   Priority() { return pri; }
  Packet   Input() { return wkq; }
  TaskRec Handle() { return handle; }

  void     SetLink(Task x)    { link = x; }
  void    SetIdent(int x)     { id = x; }
  void SetPriority(int x)     { pri = x; }
  void    SetInput(Packet x)  { wkq = x; }
  void   SetHandle(TaskRec x) { handle = x; }

  static final int TaskTabSize = 10;
  private static Task[] taskTab = new Task[TaskTabSize];
  static Task    TaskTab(int i)         { return taskTab[i]; }
  static void SetTaskTab(int i, Task t) { taskTab[i] = t; }

  static Task taskList;

  static final boolean tracing = false;
  private static int holdCount = 0;
  private static int qpktCount = 0;

  static int  get_holdCount()      { return holdCount; }
  static void set_holdCount(int i) { holdCount = i; }

  static int  get_queuePacketCount()      { return qpktCount; }
  static void set_queuePacketCount(int i) { qpktCount = i; }

  Task(int i, int p, Packet w, TaskState initialState, TaskRec r) {
    link = taskList;
    SetIdent(i);
    SetPriority(p);
    SetInput(w);
    SetPacketPending(initialState.IsPacketPending());
    SetTaskWaiting(initialState.IsTaskWaiting());
    SetTaskHolding(initialState.IsTaskHolding());
    SetHandle(r);
    taskList = this;
    SetTaskTab(i, this);
  }

  abstract Task fn(Packet pkt, TaskRec r);

  private Task AddPacket(Packet p, Task old) {
    if (Input() == null) {
      SetInput(p);
      SetPacketPending(true);
      if (Priority() > old.Priority())
        return this;
    } else {
      p.append_to(Input());
    }
    return old;
  }

  Task RunTask() {
    Packet msg;

    if (IsWaitingWithPacket()) {
      msg = Input();
      SetInput(msg.Link());
      if (Input() == null)
	Running();
      else
	PacketPending();
    } else {
      msg = null;
    }
    return fn(msg, Handle());
  }

  protected Task waitTask() {
    SetTaskWaiting(true);
    return this;
  }

  protected Task hold() {
    set_holdCount(get_holdCount() + 1);
    SetTaskHolding(true);
    return Link();
  }

  protected Task release(int i) {
    Task t = findtcb(i);
    t.SetTaskHolding(false);
    return t.Priority() > Priority() ? t : this;
  }

  protected Task qpkt(Packet pkt) {
    Task t = findtcb(pkt.Ident());
    set_queuePacketCount(get_queuePacketCount() + 1);
    pkt.SetLink(null);
    pkt.SetIdent(Ident());
    return t.AddPacket(pkt, this);
  }

  static Task findtcb(int id) {
    Task t = Task.TaskTab(id);
    if (t == null) 
      System.out.println("\nBad task id " + id);
    return t;
  }

  static void trace(char a) {
    if (--layout <= 0) {
      System.out.println();
      layout = 50;
    }
    System.out.print(a);
  }

}

//----- DeviceTask ---------------------------------------------------

class DeviceTask extends Task
{
  DeviceTask(int i, int p, Packet w, TaskState s, TaskRec r) {
    super(i, p, w, s, r);
  }

  Task fn(Packet pkt, TaskRec r) {
    DeviceTaskRec d = (DeviceTaskRec)r;
    if (pkt == null) {
      pkt = d.Pending();
      if (pkt == null) 
        return waitTask();
      else {
	d.SetPending(null);
	return qpkt(pkt);
      }
    } else {
      d.SetPending(pkt);
      if (tracing) trace((char)pkt.Datum());
      return hold();
    }
  }
}


//----- HandlerTask --------------------------------------------------

class HandlerTask extends Task
{
  HandlerTask(int i, int p, Packet w, TaskState s, TaskRec r) {
    super(i, p, w, s, r);
  }

  Task fn(Packet pkt, TaskRec r) {
    HandlerTaskRec h = (HandlerTaskRec)r;
    if (pkt != null) {
      if (pkt.Kind() == Richards.K_WORK)
	h.WorkInAdd(pkt);
      else
        h.DeviceInAdd(pkt);
    }
    Packet work = h.WorkIn();
    if (work == null)
      return waitTask();

    int count = work.Datum();

    if (count >= Packet.BUFSIZE) {
      h.SetWorkIn(work.Link());
      return qpkt(work);
    }

    Packet dev = h.DeviceIn();
    if (dev == null)
      return waitTask();

    h.SetDeviceIn(dev.Link());
    dev.SetDatum(work.Data(count));
    work.SetDatum(count + 1);
    return qpkt(dev);
  }
}


//----- IdleTask -----------------------------------------------------

class IdleTask extends Task 
{
  IdleTask(int i, int a1, int a2, TaskState s, TaskRec r) {
    super(i, 0, null, s, r);
  }

  Task fn(Packet pkt, TaskRec r) {
    IdleTaskRec i = (IdleTaskRec)r;

    i.SetCount(i.Count() - 1);
    if (i.Count() == 0) {
      return hold();
    } else if ((i.Control() & 1) == 0) {
      i.SetControl(i.Control() / 2);
      return release(Richards.I_DEVA);
    } else {
      i.SetControl((i.Control() / 2) ^ 0XD008);
      return release(Richards.I_DEVB);
    }
  }

}


//----- WorkTask -----------------------------------------------------

class WorkTask extends Task 
{
  WorkTask(int i, int p, Packet w, TaskState s, TaskRec r) {
    super(i, p, w, s, r);
  }

  Task fn(Packet pkt, TaskRec r) {
    WorkerTaskRec w = (WorkerTaskRec)r;

    if (pkt == null)
      return waitTask();

    int dest = (w.Destination() == Richards.I_HANDLERA
		? Richards.I_HANDLERB
		: Richards.I_HANDLERA);
    w.SetDestination(dest);
    pkt.SetIdent(dest);
    pkt.SetDatum(0);
    for (int i = 0; i < Packet.BUFSIZE; i++) { 
      w.SetCount(w.Count() + 1);
      if (w.Count() > 26) w.SetCount(1);
      pkt.SetData(i, 'A' + w.Count() - 1);
    }
    return qpkt(pkt);
  }
}


//----- Richards -----------------------------------------------------


public class Richards implements Benchmark
{
  private long total_ms;
  public long getRunTime() { return total_ms; }

  public static void main(String[] args) {
    (new Richards()).inst_main(args);
  }

  static int iterations = 10;

  public void inst_main(String[] args) { 
    System.out.println("Richards benchmark (deutsch_acc_virtual) starting...");
    long startTime = System.currentTimeMillis();
    if (!run())
      return;
    long endTime = System.currentTimeMillis();
    System.out.println("finished.");
    total_ms= endTime - startTime;
    System.out.println("Total time for " + iterations + " iterations: "
		       + (total_ms/1000.0) + " secs");
    System.out.println("Average time per iteration: "
		       + (total_ms / iterations) + " ms");
  }

  static void schedule() {
    Task t = Task.taskList;
    while (t != null) {
      Packet pkt = null;

      if (Task.tracing) 
	System.out.println("tcb=" + t.Ident());

      if (t.IsTaskHoldingOrWaiting()) 
        t = t.Link();
      else {
        if (Task.tracing) Task.trace((char)('0' + t.Ident()));
        t = t.RunTask();
      }
    }
  }

  public boolean run() {
    for (int i= 0; i < iterations; i++){
      Task.set_holdCount(0);
      Task.set_queuePacketCount(0);  // Added to allow repeated execution
				     // of the test.    Ole Agesen, 3/95.

      new IdleTask(I_IDLE, 1, 10000, (new TaskState()).Running(),
		   new IdleTaskRec());

      Packet wkq = new Packet(null, 0, K_WORK);
      wkq = new Packet(wkq, 0, K_WORK);
      new WorkTask(I_WORK, 1000, wkq,
		   (new TaskState()).WaitingWithPacket(),
		   new WorkerTaskRec());

      wkq = new Packet(null, I_DEVA, K_DEV);
      wkq = new Packet(wkq, I_DEVA, K_DEV);
      wkq = new Packet(wkq, I_DEVA, K_DEV);
      new HandlerTask(I_HANDLERA, 2000, wkq,
		      (new TaskState()).WaitingWithPacket(),
		      new HandlerTaskRec());

      wkq = new Packet(null, I_DEVB, K_DEV);
      wkq = new Packet(wkq, I_DEVB, K_DEV);
      wkq = new Packet(wkq, I_DEVB, K_DEV);
      new HandlerTask(I_HANDLERB, 3000, wkq,
		      (new TaskState()).WaitingWithPacket(),
		      new HandlerTaskRec());

      wkq = null;
      new DeviceTask(I_DEVA, 4000, wkq, (new TaskState()).Waiting(),
		     new DeviceTaskRec());
      new DeviceTask(I_DEVB, 5000, wkq, (new TaskState()).Waiting(),
		     new DeviceTaskRec());

      schedule();

      if (Task.get_queuePacketCount() == 23246 && Task.get_holdCount() == 9297) 
        ; // correct
      else {
        System.out.println("Incorrect results!");
        return false;
      }
    }
    return true;
  }

  // Task IDs
  static final int
    I_IDLE = 1,
    I_WORK = 2,
    I_HANDLERA = 3,
    I_HANDLERB = 4,
    I_DEVA = 5,
    I_DEVB = 6;

  // Packet types
  static final int
    K_DEV = 1000,
    K_WORK = 1001;
}
/* 
compilationUnit
  packageDcl( \/\/  Based on original version written in BCPL by Dr Martin Richards \/\/  in 1981 at Cambridge University Computer Laboratory, England \/\/  and a C++ version derived from a Smalltalk version written by \/\/  L Peter Deutsch. \/\/  Java version:  Copyright (C) 1995 Sun Microsystems, Inc. \/\/  Translation from C++, Mario Wolczko \/\/  Outer loop added by Alex Jacoby )  package
    nameNode COM . sun . labs . kanban . richards_deutsch_acc_virtual ;
  importDcl import
    nameNode Benchmark ;
  classDcl( \/\/----- Packet ------------------------------------------------------- )  class Packet
    extendsClass
    implementsInterfaces
    classBody {
      varDclsStatement
        modifiers static final
        basicType int
        variableDeclarator BUFSIZE =
          scalarInitializer 4 ;
      varDclsStatement
        modifiers private
        classOrInterfaceType
          nameNode Packet
        variableDeclarator link ;
      varDclsStatement
        modifiers private
        basicType int
        variableDeclarator id ;
      varDclsStatement
        modifiers private
        basicType int
        variableDeclarator kind ;
      varDclsStatement
        modifiers private
        basicType int
        variableDeclarator datum ;
      varDclsStatement
        modifiers private
        arrayType
          basicType int
          squareList [ ]
        variableDeclarator data =
          scalarInitializer
            newArray new
              basicType int
              dimensionExpression [
                nameNode BUFSIZE ] ;
      constructorDcl
        classOrInterfaceType
          nameNode Packet
        attributeName Packet
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet l ,
          formalParameter
            basicType int i ,
          formalParameter
            basicType int k )
        throws
        block {
          expressionStatement
            methodInvocation
              nameNode SetLink
              argumentList (
                argument
                  nameNode l ) ;
          expressionStatement
            methodInvocation
              nameNode SetIdent
              argumentList (
                argument
                  nameNode i ) ;
          expressionStatement
            methodInvocation
              nameNode SetKind
              argumentList (
                argument
                  nameNode k ) ;
          expressionStatement
            methodInvocation
              nameNode SetDatum
              argumentList (
                argument 0 ) ;
          forStatement for
            forParenList (
              forInit
                varDclsStatement
                  basicType int
                  variableDeclarator j =
                    scalarInitializer 0 ;
              forCond
                infixExpression
                  nameNode j <
                  nameNode BUFSIZE ;
              forUpdate
                statementExpressionList
                  postfixExpression
                    nameNode j ++ )
            expressionStatement
              methodInvocation
                nameNode SetData
                argumentList (
                  argument
                    nameNode j ,
                  argument 0 ) ; }
      methodDcl
        classOrInterfaceType
          nameNode Packet
        attributeName Link
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode link ; }
      methodDcl
        basicType int
        attributeName Ident
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode id ; }
      methodDcl
        basicType int
        attributeName Kind
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode kind ; }
      methodDcl
        basicType int
        attributeName Datum
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode datum ; }
      methodDcl
        basicType void
        attributeName SetLink
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet l )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode link =
              nameNode l ; }
      methodDcl
        basicType void
        attributeName SetIdent
        formalParameterList (
          formalParameter
            basicType int i )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode id =
              nameNode i ; }
      methodDcl
        basicType void
        attributeName SetKind
        formalParameterList (
          formalParameter
            basicType int k )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode kind =
              nameNode k ; }
      methodDcl
        basicType void
        attributeName SetDatum
        formalParameterList (
          formalParameter
            basicType int d )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode datum =
              nameNode d ; }
      methodDcl
        basicType int
        attributeName Data
        formalParameterList (
          formalParameter
            basicType int i )
        throws
        block {
          returnStatement return
            arrayAccess
              nameNode data
              dimensionExpression [
                nameNode i ] ; }
      methodDcl
        basicType void
        attributeName SetData
        formalParameterList (
          formalParameter
            basicType int i ,
          formalParameter
            basicType int d )
        throws
        block {
          expressionStatement
            infixExpression
              arrayAccess
                nameNode data
                dimensionExpression [
                  nameNode i ] =
              nameNode d ; }
      methodDcl
        classOrInterfaceType
          nameNode Packet
        attributeName append_to
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet list )
        throws
        block {
          expressionStatement
            methodInvocation
              nameNode SetLink
              argumentList (
                argument null ) ;
          ifStatement if
            parenList (
              infixExpression
                nameNode list == null )
            returnStatement return this ; else
            blockStatement
              block {
                varDclsStatement
                  classOrInterfaceType
                    nameNode Packet
                  variableDeclarator p =
                    scalarInitializer
                      nameNode list ;
                varDclsStatement
                  classOrInterfaceType
                    nameNode Packet
                  variableDeclarator next =
                    scalarInitializer
                      methodInvocation
                        nameNode p . Link
                        argumentList ( ) ;
                whileStatement while
                  parenList (
                    infixExpression
                      nameNode next != null )
                  blockStatement
                    block {
                      expressionStatement
                        infixExpression
                          nameNode p =
                          nameNode next ;
                      expressionStatement
                        infixExpression
                          nameNode next =
                          methodInvocation
                            nameNode p . Link
                            argumentList ( ) ; }
                expressionStatement
                  methodInvocation
                    nameNode p . SetLink
                    argumentList (
                      argument this ) ;
                returnStatement return
                  nameNode list ; } } }
  classDcl
    modifiers( \/\/----- Task Records------------------------------ )  abstract class TaskRec
    extendsClass
    implementsInterfaces
    classBody { } ( \/\/ so we have a common type for all task records ) 
  classDcl class DeviceTaskRec
    extendsClass extends
      classOrInterfaceName TaskRec
    implementsInterfaces
    classBody {
      varDclsStatement
        modifiers private
        classOrInterfaceType
          nameNode Packet
        variableDeclarator pending ;
      constructorDcl
        classOrInterfaceType
          nameNode DeviceTaskRec
        attributeName DeviceTaskRec
        formalParameterList ( )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode pending = null ; }
      methodDcl
        classOrInterfaceType
          nameNode Packet
        attributeName Pending
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode pending ; }
      methodDcl
        basicType void
        attributeName SetPending
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet p )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode pending =
              nameNode p ; } }
  classDcl class IdleTaskRec
    extendsClass extends
      classOrInterfaceName TaskRec
    implementsInterfaces
    classBody {
      varDclsStatement
        modifiers private
        basicType int
        variableDeclarator control ,
        variableDeclarator count ;
      constructorDcl
        classOrInterfaceType
          nameNode IdleTaskRec
        attributeName IdleTaskRec
        formalParameterList ( )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode control = 1 ;
          expressionStatement
            infixExpression
              nameNode count = 10000 ; }
      methodDcl
        basicType int
        attributeName Control
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode control ; }
      methodDcl
        basicType int
        attributeName Count
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode count ; }
      methodDcl
        basicType void
        attributeName SetControl
        formalParameterList (
          formalParameter
            basicType int n )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode control =
              nameNode n ; }
      methodDcl
        basicType void
        attributeName SetCount
        formalParameterList (
          formalParameter
            basicType int n )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode count =
              nameNode n ; } }
  classDcl class HandlerTaskRec
    extendsClass extends
      classOrInterfaceName TaskRec
    implementsInterfaces
    classBody {
      varDclsStatement
        modifiers private
        classOrInterfaceType
          nameNode Packet
        variableDeclarator workIn ,
        variableDeclarator deviceIn ;
      constructorDcl
        classOrInterfaceType
          nameNode HandlerTaskRec
        attributeName HandlerTaskRec
        formalParameterList ( )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode workIn =
              infixExpression
                nameNode deviceIn = null ; }
      methodDcl
        classOrInterfaceType
          nameNode Packet
        attributeName WorkIn
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode workIn ; }
      methodDcl
        classOrInterfaceType
          nameNode Packet
        attributeName DeviceIn
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode deviceIn ; }
      methodDcl
        basicType void
        attributeName SetDeviceIn
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet p )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode deviceIn =
              nameNode p ; }
      methodDcl
        basicType void
        attributeName SetWorkIn
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet p )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode workIn =
              nameNode p ; }
      methodDcl
        classOrInterfaceType
          nameNode Packet
        attributeName WorkInAdd
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet p )
        throws
        block {
          returnStatement return
            infixExpression
              nameNode workIn =
              methodInvocation
                nameNode p . append_to
                argumentList (
                  argument
                    nameNode workIn ) ; }
      methodDcl
        classOrInterfaceType
          nameNode Packet
        attributeName DeviceInAdd
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet p )
        throws
        block {
          returnStatement return
            infixExpression
              nameNode deviceIn =
              methodInvocation
                nameNode p . append_to
                argumentList (
                  argument
                    nameNode deviceIn ) ; } }
  classDcl class WorkerTaskRec
    extendsClass extends
      classOrInterfaceName TaskRec
    implementsInterfaces
    classBody {
      varDclsStatement
        modifiers private
        basicType int
        variableDeclarator destination ;
      varDclsStatement
        modifiers private
        basicType int
        variableDeclarator count ;
      constructorDcl
        classOrInterfaceType
          nameNode WorkerTaskRec
        attributeName WorkerTaskRec
        formalParameterList ( )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode destination =
              nameNode Richards . I_HANDLERA ;
          expressionStatement
            infixExpression
              nameNode count = 0 ; }
      methodDcl
        basicType int
        attributeName Count
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode count ; }
      methodDcl
        basicType int
        attributeName Destination
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode destination ; }
      methodDcl
        basicType void
        attributeName SetCount
        formalParameterList (
          formalParameter
            basicType int n )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode count =
              nameNode n ; }
      methodDcl
        basicType void
        attributeName SetDestination
        formalParameterList (
          formalParameter
            basicType int d )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode destination =
              nameNode d ; } }
  classDcl( \/\/----- Task --------------------------------------------------------- )  class TaskState
    extendsClass
    implementsInterfaces
    classBody {
      varDclsStatement
        modifiers protected
        basicType boolean
        variableDeclarator packetPending ,
        variableDeclarator taskWaiting ,
        variableDeclarator taskHolding ;
      constructorDcl
        classOrInterfaceType
          nameNode TaskState
        attributeName TaskState
        formalParameterList ( )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode packetPending = true ;
          expressionStatement
            infixExpression
              nameNode taskWaiting = false ;
          expressionStatement
            infixExpression
              nameNode taskHolding = false ; }
      methodDcl
        classOrInterfaceType
          nameNode TaskState
        attributeName PacketPending
        formalParameterList ( )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode packetPending = true ;
          expressionStatement
            infixExpression
              nameNode taskWaiting =
              infixExpression
                nameNode taskHolding = false ;
          returnStatement return this ; }
      methodDcl
        classOrInterfaceType
          nameNode TaskState
        attributeName Waiting
        formalParameterList ( )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode packetPending =
              infixExpression
                nameNode taskHolding = false ;
          expressionStatement
            infixExpression
              nameNode taskWaiting = true ;
          returnStatement return this ; }
      methodDcl
        classOrInterfaceType
          nameNode TaskState
        attributeName Running
        formalParameterList ( )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode packetPending =
              infixExpression
                nameNode taskWaiting =
                infixExpression
                  nameNode taskHolding = false ;
          returnStatement return this ; }
      methodDcl
        classOrInterfaceType
          nameNode TaskState
        attributeName WaitingWithPacket
        formalParameterList ( )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode packetPending =
              infixExpression
                nameNode taskWaiting = true ;
          expressionStatement
            infixExpression
              nameNode taskHolding = false ;
          returnStatement return this ; }
      methodDcl
        basicType( \/\* accessing \*\/ )  boolean
        attributeName IsPacketPending
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode packetPending ; }
      methodDcl
        basicType boolean
        attributeName IsTaskWaiting
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode taskWaiting ; }
      methodDcl
        basicType boolean
        attributeName IsTaskHolding
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode taskHolding ; }
      methodDcl
        basicType void
        attributeName SetTaskHolding
        formalParameterList (
          formalParameter
            basicType boolean state )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode taskHolding =
              nameNode state ; }
      methodDcl
        basicType void
        attributeName SetTaskWaiting
        formalParameterList (
          formalParameter
            basicType boolean state )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode taskWaiting =
              nameNode state ; }
      methodDcl
        basicType void
        attributeName SetPacketPending
        formalParameterList (
          formalParameter
            basicType boolean state )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode packetPending =
              nameNode state ; }
      methodDcl
        basicType( \/\* testing \*\/ )  boolean
        attributeName IsTaskHoldingOrWaiting
        formalParameterList ( )
        throws
        block {
          returnStatement return
            infixExpression
              infixExpression
                methodInvocation
                  nameNode IsTaskHolding
                  argumentList ( ) ||
                prefixExpression !
                  methodInvocation
                    nameNode IsPacketPending
                    argumentList ( ) &&
              methodInvocation
                nameNode IsTaskWaiting
                argumentList ( ) ; }
      methodDcl
        basicType boolean
        attributeName IsWaitingWithPacket
        formalParameterList ( )
        throws
        block {
          returnStatement return
            infixExpression
              infixExpression
                methodInvocation
                  nameNode IsPacketPending
                  argumentList ( ) &&
                methodInvocation
                  nameNode IsTaskWaiting
                  argumentList ( ) &&
              prefixExpression !
                methodInvocation
                  nameNode IsTaskHolding
                  argumentList ( ) ; } }
  classDcl
    modifiers abstract class Task
    extendsClass extends
      classOrInterfaceName TaskState
    implementsInterfaces
    classBody {
      varDclsStatement
        modifiers static
        basicType int
        variableDeclarator layout =
          scalarInitializer 0 ;
      varDclsStatement
        modifiers protected
        classOrInterfaceType
          nameNode Task
        variableDeclarator link ;
      varDclsStatement
        modifiers protected
        basicType int
        variableDeclarator id ;
      varDclsStatement
        modifiers protected
        basicType int
        variableDeclarator pri ;
      varDclsStatement
        modifiers protected
        classOrInterfaceType
          nameNode Packet
        variableDeclarator wkq ;
      varDclsStatement
        modifiers protected
        classOrInterfaceType
          nameNode TaskRec
        variableDeclarator handle ;
      methodDcl
        classOrInterfaceType
          nameNode Task
        attributeName Link
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode link ; }
      methodDcl
        basicType int
        attributeName Ident
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode id ; }
      methodDcl
        basicType int
        attributeName Priority
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode pri ; }
      methodDcl
        classOrInterfaceType
          nameNode Packet
        attributeName Input
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode wkq ; }
      methodDcl
        classOrInterfaceType
          nameNode TaskRec
        attributeName Handle
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode handle ; }
      methodDcl
        basicType void
        attributeName SetLink
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Task x )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode link =
              nameNode x ; }
      methodDcl
        basicType void
        attributeName SetIdent
        formalParameterList (
          formalParameter
            basicType int x )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode id =
              nameNode x ; }
      methodDcl
        basicType void
        attributeName SetPriority
        formalParameterList (
          formalParameter
            basicType int x )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode pri =
              nameNode x ; }
      methodDcl
        basicType void
        attributeName SetInput
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet x )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode wkq =
              nameNode x ; }
      methodDcl
        basicType void
        attributeName SetHandle
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode TaskRec x )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode handle =
              nameNode x ; }
      varDclsStatement
        modifiers static final
        basicType int
        variableDeclarator TaskTabSize =
          scalarInitializer 10 ;
      varDclsStatement
        modifiers private static
        arrayType
          classOrInterfaceType
            nameNode Task
          squareList [ ]
        variableDeclarator taskTab =
          scalarInitializer
            newArray new
              classOrInterfaceType
                nameNode Task
              dimensionExpression [
                nameNode TaskTabSize ] ;
      methodDcl
        modifiers static
        classOrInterfaceType
          nameNode Task
        attributeName TaskTab
        formalParameterList (
          formalParameter
            basicType int i )
        throws
        block {
          returnStatement return
            arrayAccess
              nameNode taskTab
              dimensionExpression [
                nameNode i ] ; }
      methodDcl
        modifiers static
        basicType void
        attributeName SetTaskTab
        formalParameterList (
          formalParameter
            basicType int i ,
          formalParameter
            classOrInterfaceType
              nameNode Task t )
        throws
        block {
          expressionStatement
            infixExpression
              arrayAccess
                nameNode taskTab
                dimensionExpression [
                  nameNode i ] =
              nameNode t ; }
      varDclsStatement
        modifiers static
        classOrInterfaceType
          nameNode Task
        variableDeclarator taskList ;
      varDclsStatement
        modifiers static final
        basicType boolean
        variableDeclarator tracing =
          scalarInitializer false ;
      varDclsStatement
        modifiers private static
        basicType int
        variableDeclarator holdCount =
          scalarInitializer 0 ;
      varDclsStatement
        modifiers private static
        basicType int
        variableDeclarator qpktCount =
          scalarInitializer 0 ;
      methodDcl
        modifiers static
        basicType int
        attributeName get_holdCount
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode holdCount ; }
      methodDcl
        modifiers static
        basicType void
        attributeName set_holdCount
        formalParameterList (
          formalParameter
            basicType int i )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode holdCount =
              nameNode i ; }
      methodDcl
        modifiers static
        basicType int
        attributeName get_queuePacketCount
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode qpktCount ; }
      methodDcl
        modifiers static
        basicType void
        attributeName set_queuePacketCount
        formalParameterList (
          formalParameter
            basicType int i )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode qpktCount =
              nameNode i ; }
      constructorDcl
        classOrInterfaceType
          nameNode Task
        attributeName Task
        formalParameterList (
          formalParameter
            basicType int i ,
          formalParameter
            basicType int p ,
          formalParameter
            classOrInterfaceType
              nameNode Packet w ,
          formalParameter
            classOrInterfaceType
              nameNode TaskState initialState ,
          formalParameter
            classOrInterfaceType
              nameNode TaskRec r )
        throws
        block {
          expressionStatement
            infixExpression
              nameNode link =
              nameNode taskList ;
          expressionStatement
            methodInvocation
              nameNode SetIdent
              argumentList (
                argument
                  nameNode i ) ;
          expressionStatement
            methodInvocation
              nameNode SetPriority
              argumentList (
                argument
                  nameNode p ) ;
          expressionStatement
            methodInvocation
              nameNode SetInput
              argumentList (
                argument
                  nameNode w ) ;
          expressionStatement
            methodInvocation
              nameNode SetPacketPending
              argumentList (
                argument
                  methodInvocation
                    nameNode initialState . IsPacketPending
                    argumentList ( ) ) ;
          expressionStatement
            methodInvocation
              nameNode SetTaskWaiting
              argumentList (
                argument
                  methodInvocation
                    nameNode initialState . IsTaskWaiting
                    argumentList ( ) ) ;
          expressionStatement
            methodInvocation
              nameNode SetTaskHolding
              argumentList (
                argument
                  methodInvocation
                    nameNode initialState . IsTaskHolding
                    argumentList ( ) ) ;
          expressionStatement
            methodInvocation
              nameNode SetHandle
              argumentList (
                argument
                  nameNode r ) ;
          expressionStatement
            infixExpression
              nameNode taskList = this ;
          expressionStatement
            methodInvocation
              nameNode SetTaskTab
              argumentList (
                argument
                  nameNode i ,
                argument this ) ; }
      methodDcl
        modifiers abstract
        classOrInterfaceType
          nameNode Task
        attributeName fn
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet pkt ,
          formalParameter
            classOrInterfaceType
              nameNode TaskRec r )
        throws ;
      methodDcl
        modifiers private
        classOrInterfaceType
          nameNode Task
        attributeName AddPacket
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet p ,
          formalParameter
            classOrInterfaceType
              nameNode Task old )
        throws
        block {
          ifStatement if
            parenList (
              infixExpression
                methodInvocation
                  nameNode Input
                  argumentList ( ) == null )
            blockStatement
              block {
                expressionStatement
                  methodInvocation
                    nameNode SetInput
                    argumentList (
                      argument
                        nameNode p ) ;
                expressionStatement
                  methodInvocation
                    nameNode SetPacketPending
                    argumentList (
                      argument true ) ;
                ifStatement if
                  parenList (
                    infixExpression
                      methodInvocation
                        nameNode Priority
                        argumentList ( ) >
                      methodInvocation
                        nameNode old . Priority
                        argumentList ( ) )
                  returnStatement return this ; } else
            blockStatement
              block {
                expressionStatement
                  methodInvocation
                    nameNode p . append_to
                    argumentList (
                      argument
                        methodInvocation
                          nameNode Input
                          argumentList ( ) ) ; }
          returnStatement return
            nameNode old ; }
      methodDcl
        classOrInterfaceType
          nameNode Task
        attributeName RunTask
        formalParameterList ( )
        throws
        block {
          varDclsStatement
            classOrInterfaceType
              nameNode Packet
            variableDeclarator msg ;
          ifStatement if
            parenList (
              methodInvocation
                nameNode IsWaitingWithPacket
                argumentList ( ) )
            blockStatement
              block {
                expressionStatement
                  infixExpression
                    nameNode msg =
                    methodInvocation
                      nameNode Input
                      argumentList ( ) ;
                expressionStatement
                  methodInvocation
                    nameNode SetInput
                    argumentList (
                      argument
                        methodInvocation
                          nameNode msg . Link
                          argumentList ( ) ) ;
                ifStatement if
                  parenList (
                    infixExpression
                      methodInvocation
                        nameNode Input
                        argumentList ( ) == null )
                  expressionStatement
                    methodInvocation
                      nameNode Running
                      argumentList ( ) ; else
                  expressionStatement
                    methodInvocation
                      nameNode PacketPending
                      argumentList ( ) ; } else
            blockStatement
              block {
                expressionStatement
                  infixExpression
                    nameNode msg = null ; }
          returnStatement return
            methodInvocation
              nameNode fn
              argumentList (
                argument
                  nameNode msg ,
                argument
                  methodInvocation
                    nameNode Handle
                    argumentList ( ) ) ; }
      methodDcl
        modifiers protected
        classOrInterfaceType
          nameNode Task
        attributeName waitTask
        formalParameterList ( )
        throws
        block {
          expressionStatement
            methodInvocation
              nameNode SetTaskWaiting
              argumentList (
                argument true ) ;
          returnStatement return this ; }
      methodDcl
        modifiers protected
        classOrInterfaceType
          nameNode Task
        attributeName hold
        formalParameterList ( )
        throws
        block {
          expressionStatement
            methodInvocation
              nameNode set_holdCount
              argumentList (
                argument
                  infixExpression
                    methodInvocation
                      nameNode get_holdCount
                      argumentList ( ) + 1 ) ;
          expressionStatement
            methodInvocation
              nameNode SetTaskHolding
              argumentList (
                argument true ) ;
          returnStatement return
            methodInvocation
              nameNode Link
              argumentList ( ) ; }
      methodDcl
        modifiers protected
        classOrInterfaceType
          nameNode Task
        attributeName release
        formalParameterList (
          formalParameter
            basicType int i )
        throws
        block {
          varDclsStatement
            classOrInterfaceType
              nameNode Task
            variableDeclarator t =
              scalarInitializer
                methodInvocation
                  nameNode findtcb
                  argumentList (
                    argument
                      nameNode i ) ;
          expressionStatement
            methodInvocation
              nameNode t . SetTaskHolding
              argumentList (
                argument false ) ;
          returnStatement return
            expressionIf
              infixExpression
                methodInvocation
                  nameNode t . Priority
                  argumentList ( ) >
                methodInvocation
                  nameNode Priority
                  argumentList ( ) ?
              nameNode t : this ; }
      methodDcl
        modifiers protected
        classOrInterfaceType
          nameNode Task
        attributeName qpkt
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet pkt )
        throws
        block {
          varDclsStatement
            classOrInterfaceType
              nameNode Task
            variableDeclarator t =
              scalarInitializer
                methodInvocation
                  nameNode findtcb
                  argumentList (
                    argument
                      methodInvocation
                        nameNode pkt . Ident
                        argumentList ( ) ) ;
          expressionStatement
            methodInvocation
              nameNode set_queuePacketCount
              argumentList (
                argument
                  infixExpression
                    methodInvocation
                      nameNode get_queuePacketCount
                      argumentList ( ) + 1 ) ;
          expressionStatement
            methodInvocation
              nameNode pkt . SetLink
              argumentList (
                argument null ) ;
          expressionStatement
            methodInvocation
              nameNode pkt . SetIdent
              argumentList (
                argument
                  methodInvocation
                    nameNode Ident
                    argumentList ( ) ) ;
          returnStatement return
            methodInvocation
              nameNode t . AddPacket
              argumentList (
                argument
                  nameNode pkt ,
                argument this ) ; }
      methodDcl
        modifiers static
        classOrInterfaceType
          nameNode Task
        attributeName findtcb
        formalParameterList (
          formalParameter
            basicType int id )
        throws
        block {
          varDclsStatement
            classOrInterfaceType
              nameNode Task
            variableDeclarator t =
              scalarInitializer
                methodInvocation
                  nameNode Task . TaskTab
                  argumentList (
                    argument
                      nameNode id ) ;
          ifStatement if
            parenList (
              infixExpression
                nameNode t == null )
            expressionStatement
              methodInvocation
                nameNode System . out . println
                argumentList (
                  argument
                    infixExpression '
Bad task id ' +
                      nameNode id ) ;
          returnStatement return
            nameNode t ; }
      methodDcl
        modifiers static
        basicType void
        attributeName trace
        formalParameterList (
          formalParameter
            basicType char a )
        throws
        block {
          ifStatement if
            parenList (
              infixExpression
                prefixExpression --
                  nameNode layout <= 0 )
            blockStatement
              block {
                expressionStatement
                  methodInvocation
                    nameNode System . out . println
                    argumentList ( ) ;
                expressionStatement
                  infixExpression
                    nameNode layout = 50 ; }
          expressionStatement
            methodInvocation
              nameNode System . out . print
              argumentList (
                argument
                  nameNode a ) ; } }
  classDcl( \/\/----- DeviceTask --------------------------------------------------- )  class DeviceTask
    extendsClass extends
      classOrInterfaceName Task
    implementsInterfaces
    classBody {
      constructorDcl
        classOrInterfaceType
          nameNode DeviceTask
        attributeName DeviceTask
        formalParameterList (
          formalParameter
            basicType int i ,
          formalParameter
            basicType int p ,
          formalParameter
            classOrInterfaceType
              nameNode Packet w ,
          formalParameter
            classOrInterfaceType
              nameNode TaskState s ,
          formalParameter
            classOrInterfaceType
              nameNode TaskRec r )
        throws
        block {
          expressionStatement
            superInvocation super
              argumentList (
                argument
                  nameNode i ,
                argument
                  nameNode p ,
                argument
                  nameNode w ,
                argument
                  nameNode s ,
                argument
                  nameNode r ) ; }
      methodDcl
        classOrInterfaceType
          nameNode Task
        attributeName fn
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet pkt ,
          formalParameter
            classOrInterfaceType
              nameNode TaskRec r )
        throws
        block {
          varDclsStatement
            classOrInterfaceType
              nameNode DeviceTaskRec
            variableDeclarator d =
              scalarInitializer
                prefixExpression
                  coersion (
                    classOrInterfaceType
                      nameNode DeviceTaskRec )
                  nameNode r ;
          ifStatement if
            parenList (
              infixExpression
                nameNode pkt == null )
            blockStatement
              block {
                expressionStatement
                  infixExpression
                    nameNode pkt =
                    methodInvocation
                      nameNode d . Pending
                      argumentList ( ) ;
                ifStatement if
                  parenList (
                    infixExpression
                      nameNode pkt == null )
                  returnStatement return
                    methodInvocation
                      nameNode waitTask
                      argumentList ( ) ; else
                  blockStatement
                    block {
                      expressionStatement
                        methodInvocation
                          nameNode d . SetPending
                          argumentList (
                            argument null ) ;
                      returnStatement return
                        methodInvocation
                          nameNode qpkt
                          argumentList (
                            argument
                              nameNode pkt ) ; } } else
            blockStatement
              block {
                expressionStatement
                  methodInvocation
                    nameNode d . SetPending
                    argumentList (
                      argument
                        nameNode pkt ) ;
                ifStatement if
                  parenList (
                    nameNode tracing )
                  expressionStatement
                    methodInvocation
                      nameNode trace
                      argumentList (
                        argument
                          prefixExpression
                            coersion (
                              basicType char )
                            methodInvocation
                              nameNode pkt . Datum
                              argumentList ( ) ) ;
                returnStatement return
                  methodInvocation
                    nameNode hold
                    argumentList ( ) ; } } }
  classDcl( \/\/----- HandlerTask -------------------------------------------------- )  class HandlerTask
    extendsClass extends
      classOrInterfaceName Task
    implementsInterfaces
    classBody {
      constructorDcl
        classOrInterfaceType
          nameNode HandlerTask
        attributeName HandlerTask
        formalParameterList (
          formalParameter
            basicType int i ,
          formalParameter
            basicType int p ,
          formalParameter
            classOrInterfaceType
              nameNode Packet w ,
          formalParameter
            classOrInterfaceType
              nameNode TaskState s ,
          formalParameter
            classOrInterfaceType
              nameNode TaskRec r )
        throws
        block {
          expressionStatement
            superInvocation super
              argumentList (
                argument
                  nameNode i ,
                argument
                  nameNode p ,
                argument
                  nameNode w ,
                argument
                  nameNode s ,
                argument
                  nameNode r ) ; }
      methodDcl
        classOrInterfaceType
          nameNode Task
        attributeName fn
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet pkt ,
          formalParameter
            classOrInterfaceType
              nameNode TaskRec r )
        throws
        block {
          varDclsStatement
            classOrInterfaceType
              nameNode HandlerTaskRec
            variableDeclarator h =
              scalarInitializer
                prefixExpression
                  coersion (
                    classOrInterfaceType
                      nameNode HandlerTaskRec )
                  nameNode r ;
          ifStatement if
            parenList (
              infixExpression
                nameNode pkt != null )
            blockStatement
              block {
                ifStatement if
                  parenList (
                    infixExpression
                      methodInvocation
                        nameNode pkt . Kind
                        argumentList ( ) ==
                      nameNode Richards . K_WORK )
                  expressionStatement
                    methodInvocation
                      nameNode h . WorkInAdd
                      argumentList (
                        argument
                          nameNode pkt ) ; else
                  expressionStatement
                    methodInvocation
                      nameNode h . DeviceInAdd
                      argumentList (
                        argument
                          nameNode pkt ) ; }
          varDclsStatement
            classOrInterfaceType
              nameNode Packet
            variableDeclarator work =
              scalarInitializer
                methodInvocation
                  nameNode h . WorkIn
                  argumentList ( ) ;
          ifStatement if
            parenList (
              infixExpression
                nameNode work == null )
            returnStatement return
              methodInvocation
                nameNode waitTask
                argumentList ( ) ;
          varDclsStatement
            basicType int
            variableDeclarator count =
              scalarInitializer
                methodInvocation
                  nameNode work . Datum
                  argumentList ( ) ;
          ifStatement if
            parenList (
              infixExpression
                nameNode count >=
                nameNode Packet . BUFSIZE )
            blockStatement
              block {
                expressionStatement
                  methodInvocation
                    nameNode h . SetWorkIn
                    argumentList (
                      argument
                        methodInvocation
                          nameNode work . Link
                          argumentList ( ) ) ;
                returnStatement return
                  methodInvocation
                    nameNode qpkt
                    argumentList (
                      argument
                        nameNode work ) ; }
          varDclsStatement
            classOrInterfaceType
              nameNode Packet
            variableDeclarator dev =
              scalarInitializer
                methodInvocation
                  nameNode h . DeviceIn
                  argumentList ( ) ;
          ifStatement if
            parenList (
              infixExpression
                nameNode dev == null )
            returnStatement return
              methodInvocation
                nameNode waitTask
                argumentList ( ) ;
          expressionStatement
            methodInvocation
              nameNode h . SetDeviceIn
              argumentList (
                argument
                  methodInvocation
                    nameNode dev . Link
                    argumentList ( ) ) ;
          expressionStatement
            methodInvocation
              nameNode dev . SetDatum
              argumentList (
                argument
                  methodInvocation
                    nameNode work . Data
                    argumentList (
                      argument
                        nameNode count ) ) ;
          expressionStatement
            methodInvocation
              nameNode work . SetDatum
              argumentList (
                argument
                  infixExpression
                    nameNode count + 1 ) ;
          returnStatement return
            methodInvocation
              nameNode qpkt
              argumentList (
                argument
                  nameNode dev ) ; } }
  classDcl( \/\/----- IdleTask ----------------------------------------------------- )  class IdleTask
    extendsClass extends
      classOrInterfaceName Task
    implementsInterfaces
    classBody {
      constructorDcl
        classOrInterfaceType
          nameNode IdleTask
        attributeName IdleTask
        formalParameterList (
          formalParameter
            basicType int i ,
          formalParameter
            basicType int a1 ,
          formalParameter
            basicType int a2 ,
          formalParameter
            classOrInterfaceType
              nameNode TaskState s ,
          formalParameter
            classOrInterfaceType
              nameNode TaskRec r )
        throws
        block {
          expressionStatement
            superInvocation super
              argumentList (
                argument
                  nameNode i ,
                argument 0 ,
                argument null ,
                argument
                  nameNode s ,
                argument
                  nameNode r ) ; }
      methodDcl
        classOrInterfaceType
          nameNode Task
        attributeName fn
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet pkt ,
          formalParameter
            classOrInterfaceType
              nameNode TaskRec r )
        throws
        block {
          varDclsStatement
            classOrInterfaceType
              nameNode IdleTaskRec
            variableDeclarator i =
              scalarInitializer
                prefixExpression
                  coersion (
                    classOrInterfaceType
                      nameNode IdleTaskRec )
                  nameNode r ;
          expressionStatement
            methodInvocation
              nameNode i . SetCount
              argumentList (
                argument
                  infixExpression
                    methodInvocation
                      nameNode i . Count
                      argumentList ( ) - 1 ) ;
          ifStatement if
            parenList (
              infixExpression
                methodInvocation
                  nameNode i . Count
                  argumentList ( ) == 0 )
            blockStatement
              block {
                returnStatement return
                  methodInvocation
                    nameNode hold
                    argumentList ( ) ; } else
            ifStatement if
              parenList (
                infixExpression
                  parenList (
                    infixExpression
                      methodInvocation
                        nameNode i . Control
                        argumentList ( ) & 1 ) == 0 )
              blockStatement
                block {
                  expressionStatement
                    methodInvocation
                      nameNode i . SetControl
                      argumentList (
                        argument
                          infixExpression
                            methodInvocation
                              nameNode i . Control
                              argumentList ( ) \/ 2 ) ;
                  returnStatement return
                    methodInvocation
                      nameNode release
                      argumentList (
                        argument
                          nameNode Richards . I_DEVA ) ; } else
              blockStatement
                block {
                  expressionStatement
                    methodInvocation
                      nameNode i . SetControl
                      argumentList (
                        argument
                          infixExpression
                            parenList (
                              infixExpression
                                methodInvocation
                                  nameNode i . Control
                                  argumentList ( ) \/ 2 ) ^ 53256 ) ;
                  returnStatement return
                    methodInvocation
                      nameNode release
                      argumentList (
                        argument
                          nameNode Richards . I_DEVB ) ; } } }
  classDcl( \/\/----- WorkTask ----------------------------------------------------- )  class WorkTask
    extendsClass extends
      classOrInterfaceName Task
    implementsInterfaces
    classBody {
      constructorDcl
        classOrInterfaceType
          nameNode WorkTask
        attributeName WorkTask
        formalParameterList (
          formalParameter
            basicType int i ,
          formalParameter
            basicType int p ,
          formalParameter
            classOrInterfaceType
              nameNode Packet w ,
          formalParameter
            classOrInterfaceType
              nameNode TaskState s ,
          formalParameter
            classOrInterfaceType
              nameNode TaskRec r )
        throws
        block {
          expressionStatement
            superInvocation super
              argumentList (
                argument
                  nameNode i ,
                argument
                  nameNode p ,
                argument
                  nameNode w ,
                argument
                  nameNode s ,
                argument
                  nameNode r ) ; }
      methodDcl
        classOrInterfaceType
          nameNode Task
        attributeName fn
        formalParameterList (
          formalParameter
            classOrInterfaceType
              nameNode Packet pkt ,
          formalParameter
            classOrInterfaceType
              nameNode TaskRec r )
        throws
        block {
          varDclsStatement
            classOrInterfaceType
              nameNode WorkerTaskRec
            variableDeclarator w =
              scalarInitializer
                prefixExpression
                  coersion (
                    classOrInterfaceType
                      nameNode WorkerTaskRec )
                  nameNode r ;
          ifStatement if
            parenList (
              infixExpression
                nameNode pkt == null )
            returnStatement return
              methodInvocation
                nameNode waitTask
                argumentList ( ) ;
          varDclsStatement
            basicType int
            variableDeclarator dest =
              scalarInitializer
                parenList (
                  expressionIf
                    infixExpression
                      methodInvocation
                        nameNode w . Destination
                        argumentList ( ) ==
                      nameNode Richards . I_HANDLERA ?
                    nameNode Richards . I_HANDLERB :
                    nameNode Richards . I_HANDLERA ) ;
          expressionStatement
            methodInvocation
              nameNode w . SetDestination
              argumentList (
                argument
                  nameNode dest ) ;
          expressionStatement
            methodInvocation
              nameNode pkt . SetIdent
              argumentList (
                argument
                  nameNode dest ) ;
          expressionStatement
            methodInvocation
              nameNode pkt . SetDatum
              argumentList (
                argument 0 ) ;
          forStatement for
            forParenList (
              forInit
                varDclsStatement
                  basicType int
                  variableDeclarator i =
                    scalarInitializer 0 ;
              forCond
                infixExpression
                  nameNode i <
                  nameNode Packet . BUFSIZE ;
              forUpdate
                statementExpressionList
                  postfixExpression
                    nameNode i ++ )
            blockStatement
              block {
                expressionStatement
                  methodInvocation
                    nameNode w . SetCount
                    argumentList (
                      argument
                        infixExpression
                          methodInvocation
                            nameNode w . Count
                            argumentList ( ) + 1 ) ;
                ifStatement if
                  parenList (
                    infixExpression
                      methodInvocation
                        nameNode w . Count
                        argumentList ( ) > 26 )
                  expressionStatement
                    methodInvocation
                      nameNode w . SetCount
                      argumentList (
                        argument 1 ) ;
                expressionStatement
                  methodInvocation
                    nameNode pkt . SetData
                    argumentList (
                      argument
                        nameNode i ,
                      argument
                        infixExpression
                          infixExpression 'A' +
                            methodInvocation
                              nameNode w . Count
                              argumentList ( ) - 1 ) ; }
          returnStatement return
            methodInvocation
              nameNode qpkt
              argumentList (
                argument
                  nameNode pkt ) ; } }
  classDcl
    modifiers( \/\/----- Richards ----------------------------------------------------- )  public class Richards
    extendsClass
    implementsInterfaces implements
      classOrInterfaceName Benchmark
    classBody {
      varDclsStatement
        modifiers private
        basicType long
        variableDeclarator total_ms ;
      methodDcl
        modifiers public
        basicType long
        attributeName getRunTime
        formalParameterList ( )
        throws
        block {
          returnStatement return
            nameNode total_ms ; }
      methodDcl
        modifiers public static
        basicType void
        attributeName main
        formalParameterList (
          formalParameter
            arrayType
              classOrInterfaceType
                nameNode String
              squareList [ ] args )
        throws
        block {
          expressionStatement
            postfixExpression
              parenList (
                newInstance new
                  classOrInterfaceType
                    nameNode Richards
                  argumentList ( ) )
              messageSelector . inst_main
                argumentList (
                  argument
                    nameNode args ) ; }
      varDclsStatement
        modifiers static
        basicType int
        variableDeclarator iterations =
          scalarInitializer 10 ;
      methodDcl
        modifiers public
        basicType void
        attributeName inst_main
        formalParameterList (
          formalParameter
            arrayType
              classOrInterfaceType
                nameNode String
              squareList [ ] args )
        throws
        block {
          expressionStatement
            methodInvocation
              nameNode System . out . println
              argumentList (
                argument 'Richards benchmark (deutsch_acc_virtual) starting...' ) ;
          varDclsStatement
            basicType long
            variableDeclarator startTime =
              scalarInitializer
                methodInvocation
                  nameNode System . currentTimeMillis
                  argumentList ( ) ;
          ifStatement if
            parenList (
              prefixExpression !
                methodInvocation
                  nameNode run
                  argumentList ( ) )
            returnStatement return ;
          varDclsStatement
            basicType long
            variableDeclarator endTime =
              scalarInitializer
                methodInvocation
                  nameNode System . currentTimeMillis
                  argumentList ( ) ;
          expressionStatement
            methodInvocation
              nameNode System . out . println
              argumentList (
                argument 'finished.' ) ;
          expressionStatement
            infixExpression
              nameNode total_ms =
              infixExpression
                nameNode endTime -
                nameNode startTime ;
          expressionStatement
            methodInvocation
              nameNode System . out . println
              argumentList (
                argument
                  infixExpression
                    infixExpression
                      infixExpression
                        infixExpression 'Total time for ' +
                          nameNode iterations + ' iterations: ' +
                      parenList (
                        infixExpression
                          nameNode total_ms \/ 1000.0 ) + ' secs' ) ;
          expressionStatement
            methodInvocation
              nameNode System . out . println
              argumentList (
                argument
                  infixExpression
                    infixExpression 'Average time per iteration: ' +
                      parenList (
                        infixExpression
                          nameNode total_ms \/
                          nameNode iterations ) + ' ms' ) ; }
      methodDcl
        modifiers static
        basicType void
        attributeName schedule
        formalParameterList ( )
        throws
        block {
          varDclsStatement
            classOrInterfaceType
              nameNode Task
            variableDeclarator t =
              scalarInitializer
                nameNode Task . taskList ;
          whileStatement while
            parenList (
              infixExpression
                nameNode t != null )
            blockStatement
              block {
                varDclsStatement
                  classOrInterfaceType
                    nameNode Packet
                  variableDeclarator pkt =
                    scalarInitializer null ;
                ifStatement if
                  parenList (
                    nameNode Task . tracing )
                  expressionStatement
                    methodInvocation
                      nameNode System . out . println
                      argumentList (
                        argument
                          infixExpression 'tcb=' +
                            methodInvocation
                              nameNode t . Ident
                              argumentList ( ) ) ;
                ifStatement if
                  parenList (
                    methodInvocation
                      nameNode t . IsTaskHoldingOrWaiting
                      argumentList ( ) )
                  expressionStatement
                    infixExpression
                      nameNode t =
                      methodInvocation
                        nameNode t . Link
                        argumentList ( ) ; else
                  blockStatement
                    block {
                      ifStatement if
                        parenList (
                          nameNode Task . tracing )
                        expressionStatement
                          methodInvocation
                            nameNode Task . trace
                            argumentList (
                              argument
                                prefixExpression
                                  coersion (
                                    basicType char )
                                  parenList (
                                    infixExpression '0' +
                                      methodInvocation
                                        nameNode t . Ident
                                        argumentList ( ) ) ) ;
                      expressionStatement
                        infixExpression
                          nameNode t =
                          methodInvocation
                            nameNode t . RunTask
                            argumentList ( ) ; } } }
      methodDcl
        modifiers public
        basicType boolean
        attributeName run
        formalParameterList ( )
        throws
        block {
          forStatement for
            forParenList (
              forInit
                varDclsStatement
                  basicType int
                  variableDeclarator i =
                    scalarInitializer 0 ;
              forCond
                infixExpression
                  nameNode i <
                  nameNode iterations ;
              forUpdate
                statementExpressionList
                  postfixExpression
                    nameNode i ++ )
            blockStatement
              block {
                expressionStatement
                  methodInvocation
                    nameNode Task . set_holdCount
                    argumentList (
                      argument 0 ) ;
                expressionStatement
                  methodInvocation
                    nameNode Task . set_queuePacketCount
                    argumentList (
                      argument 0 ) ; ( \/\/ Added to allow repeated execution ) 
                expressionStatement
                  newInstance( \/\/ of the test.    Ole Agesen, 3\/95. )  new
                    classOrInterfaceType
                      nameNode IdleTask
                    argumentList (
                      argument
                        nameNode I_IDLE ,
                      argument 1 ,
                      argument 10000 ,
                      argument
                        postfixExpression
                          parenList (
                            newInstance new
                              classOrInterfaceType
                                nameNode TaskState
                              argumentList ( ) )
                          messageSelector . Running
                            argumentList ( ) ,
                      argument
                        newInstance new
                          classOrInterfaceType
                            nameNode IdleTaskRec
                          argumentList ( ) ) ;
                varDclsStatement
                  classOrInterfaceType
                    nameNode Packet
                  variableDeclarator wkq =
                    scalarInitializer
                      newInstance new
                        classOrInterfaceType
                          nameNode Packet
                        argumentList (
                          argument null ,
                          argument 0 ,
                          argument
                            nameNode K_WORK ) ;
                expressionStatement
                  infixExpression
                    nameNode wkq =
                    newInstance new
                      classOrInterfaceType
                        nameNode Packet
                      argumentList (
                        argument
                          nameNode wkq ,
                        argument 0 ,
                        argument
                          nameNode K_WORK ) ;
                expressionStatement
                  newInstance new
                    classOrInterfaceType
                      nameNode WorkTask
                    argumentList (
                      argument
                        nameNode I_WORK ,
                      argument 1000 ,
                      argument
                        nameNode wkq ,
                      argument
                        postfixExpression
                          parenList (
                            newInstance new
                              classOrInterfaceType
                                nameNode TaskState
                              argumentList ( ) )
                          messageSelector . WaitingWithPacket
                            argumentList ( ) ,
                      argument
                        newInstance new
                          classOrInterfaceType
                            nameNode WorkerTaskRec
                          argumentList ( ) ) ;
                expressionStatement
                  infixExpression
                    nameNode wkq =
                    newInstance new
                      classOrInterfaceType
                        nameNode Packet
                      argumentList (
                        argument null ,
                        argument
                          nameNode I_DEVA ,
                        argument
                          nameNode K_DEV ) ;
                expressionStatement
                  infixExpression
                    nameNode wkq =
                    newInstance new
                      classOrInterfaceType
                        nameNode Packet
                      argumentList (
                        argument
                          nameNode wkq ,
                        argument
                          nameNode I_DEVA ,
                        argument
                          nameNode K_DEV ) ;
                expressionStatement
                  infixExpression
                    nameNode wkq =
                    newInstance new
                      classOrInterfaceType
                        nameNode Packet
                      argumentList (
                        argument
                          nameNode wkq ,
                        argument
                          nameNode I_DEVA ,
                        argument
                          nameNode K_DEV ) ;
                expressionStatement
                  newInstance new
                    classOrInterfaceType
                      nameNode HandlerTask
                    argumentList (
                      argument
                        nameNode I_HANDLERA ,
                      argument 2000 ,
                      argument
                        nameNode wkq ,
                      argument
                        postfixExpression
                          parenList (
                            newInstance new
                              classOrInterfaceType
                                nameNode TaskState
                              argumentList ( ) )
                          messageSelector . WaitingWithPacket
                            argumentList ( ) ,
                      argument
                        newInstance new
                          classOrInterfaceType
                            nameNode HandlerTaskRec
                          argumentList ( ) ) ;
                expressionStatement
                  infixExpression
                    nameNode wkq =
                    newInstance new
                      classOrInterfaceType
                        nameNode Packet
                      argumentList (
                        argument null ,
                        argument
                          nameNode I_DEVB ,
                        argument
                          nameNode K_DEV ) ;
                expressionStatement
                  infixExpression
                    nameNode wkq =
                    newInstance new
                      classOrInterfaceType
                        nameNode Packet
                      argumentList (
                        argument
                          nameNode wkq ,
                        argument
                          nameNode I_DEVB ,
                        argument
                          nameNode K_DEV ) ;
                expressionStatement
                  infixExpression
                    nameNode wkq =
                    newInstance new
                      classOrInterfaceType
                        nameNode Packet
                      argumentList (
                        argument
                          nameNode wkq ,
                        argument
                          nameNode I_DEVB ,
                        argument
                          nameNode K_DEV ) ;
                expressionStatement
                  newInstance new
                    classOrInterfaceType
                      nameNode HandlerTask
                    argumentList (
                      argument
                        nameNode I_HANDLERB ,
                      argument 3000 ,
                      argument
                        nameNode wkq ,
                      argument
                        postfixExpression
                          parenList (
                            newInstance new
                              classOrInterfaceType
                                nameNode TaskState
                              argumentList ( ) )
                          messageSelector . WaitingWithPacket
                            argumentList ( ) ,
                      argument
                        newInstance new
                          classOrInterfaceType
                            nameNode HandlerTaskRec
                          argumentList ( ) ) ;
                expressionStatement
                  infixExpression
                    nameNode wkq = null ;
                expressionStatement
                  newInstance new
                    classOrInterfaceType
                      nameNode DeviceTask
                    argumentList (
                      argument
                        nameNode I_DEVA ,
                      argument 4000 ,
                      argument
                        nameNode wkq ,
                      argument
                        postfixExpression
                          parenList (
                            newInstance new
                              classOrInterfaceType
                                nameNode TaskState
                              argumentList ( ) )
                          messageSelector . Waiting
                            argumentList ( ) ,
                      argument
                        newInstance new
                          classOrInterfaceType
                            nameNode DeviceTaskRec
                          argumentList ( ) ) ;
                expressionStatement
                  newInstance new
                    classOrInterfaceType
                      nameNode DeviceTask
                    argumentList (
                      argument
                        nameNode I_DEVB ,
                      argument 5000 ,
                      argument
                        nameNode wkq ,
                      argument
                        postfixExpression
                          parenList (
                            newInstance new
                              classOrInterfaceType
                                nameNode TaskState
                              argumentList ( ) )
                          messageSelector . Waiting
                            argumentList ( ) ,
                      argument
                        newInstance new
                          classOrInterfaceType
                            nameNode DeviceTaskRec
                          argumentList ( ) ) ;
                expressionStatement
                  methodInvocation
                    nameNode schedule
                    argumentList ( ) ;
                ifStatement if
                  parenList (
                    infixExpression
                      infixExpression
                        infixExpression
                          methodInvocation
                            nameNode Task . get_queuePacketCount
                            argumentList ( ) == 23246 &&
                        methodInvocation
                          nameNode Task . get_holdCount
                          argumentList ( ) == 9297 )
                  emptyStatement ; ( \/\/ correct )  else
                  blockStatement
                    block {
                      expressionStatement
                        methodInvocation
                          nameNode System . out . println
                          argumentList (
                            argument 'Incorrect results!' ) ;
                      returnStatement return false ; } }
          returnStatement return true ; }
      varDclsStatement
        modifiers( \/\/ Task IDs )  static final
        basicType int
        variableDeclarator I_IDLE =
          scalarInitializer 1 ,
        variableDeclarator I_WORK =
          scalarInitializer 2 ,
        variableDeclarator I_HANDLERA =
          scalarInitializer 3 ,
        variableDeclarator I_HANDLERB =
          scalarInitializer 4 ,
        variableDeclarator I_DEVA =
          scalarInitializer 5 ,
        variableDeclarator I_DEVB =
          scalarInitializer 6 ;
      varDclsStatement
        modifiers( \/\/ Packet types )  static final
        basicType int
        variableDeclarator K_DEV =
          scalarInitializer 1000 ,
        variableDeclarator K_WORK =
          scalarInitializer 1001 ; }
 */
