/*
 * Copyright (c) 1997 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the confidential and proprietary information of Sun
 * Microsystems, Inc. ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Sun.
 * 
 * SUN MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. SUN SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 * 
 */

/**=============================================================================
 * Interpreter.java
 *
 * This class implements a Java bytecode interpreter for the 
 * Java in Java VM.
 *
 * @system      JJava (Java in Java VM)
 * @subsystem   Bytecode interpreter
 * @author      Antero Taivalsaari, Sun Labs
 * @created     14 Oct 1997
 * @see         JJava.java, Method.java, ExecutionStack.java, ...
 *============================================================================*/

import java.io.*;
import java.util.*;
import Bytecodes;
// package JJava;

public
class Interpreter extends InternalObject {

/*==============================================================================
 * Constants
 *============================================================================*/


/*==============================================================================
 * Fields
 *============================================================================*/


/*==============================================================================
 * Constructors
 *============================================================================*/

    /**
     * Prevent the instantiation of this class.
     */
    private Interpreter() {}

/*==============================================================================
 * Internal methods
 *============================================================================*/

/*==============================================================================
 * The bytecode interpreter
 *============================================================================*/

    /**
     * interpret: this is the big kabloona; bytecode interpreter for 
     * running Java bytecodes. In order to operate, there must be an
     * active thread which contains the necessary virtual machine
     * registers.
     */
    public static void Interpret() {

      while (true) {
          
        // Get the bytecode to be executed
        byte bytecode = VM.get_instruction();

        System.out.println("Executing bytecode " + Bytecodes.get_bytecode_name(bytecode));

        switch (bytecode) {

        case Bytecodes.NOP:                       // 0x00
            break; 

        case Bytecodes.ACONST_NULL:               // 0x01
            VM.push(null);
            break;

        /*============================================================================*/

        case Bytecodes.ICONST_M1:                 // 0x02;
            VM.push(new Integer(-1));
            break;

        case Bytecodes.ICONST_0:                  // 0x03;
            VM.push(new Integer(0));
            break;

        case Bytecodes.ICONST_1:                  // 0x04;
            VM.push(new Integer(1));
            break;

        case Bytecodes.ICONST_2:                  // 0x05;
            VM.push(new Integer(2));
            break;

        case Bytecodes.ICONST_3:                  // 0x06;
            VM.push(new Integer(3));
            break;

        case Bytecodes.ICONST_4:                  // 0x07;
            VM.push(new Integer(4));
            break;

        case Bytecodes.ICONST_5:                  // 0x08;
            VM.push(new Integer(5));
            break;

        /*============================================================================*/

        case Bytecodes.LCONST_0:                  // 0x09;
            VM.push(new Long(0));
            break;

        case Bytecodes.LCONST_1:                  // 0x0A;
            VM.push(new Long(1));
            break;

        /*============================================================================*/

        case Bytecodes.FCONST_0:                  // 0x0B;   
            VM.push(new Float(0));
            break;

        case Bytecodes.FCONST_1:                  // 0x0C;
            VM.push(new Float(1));
            break;

        case Bytecodes.FCONST_2:                  // 0x0D;
            VM.push(new Float(2));
            break;

        /*============================================================================*/

        case Bytecodes.DCONST_0:                  // 0x0E;
            VM.push(new Double(2));
            break;

        case Bytecodes.DCONST_1:                  // 0x0F;
            VM.push(new Double(2));
            break;

        /*============================================================================*/

        case Bytecodes.BIPUSH:                    // 0x10;
            VM.push(new Integer(VM.get_inlined_parameter_byte1()));
            VM.increment_bcp(1);
            break;

        case Bytecodes.SIPUSH:                    // 0x11;
            VM.push(new Integer(VM.get_inlined_parameter_short1()));
            VM.increment_bcp(2);
            break;
        
        /*============================================================================*/

        case Bytecodes.LDC:                       // 0x12;
            VM.increment_bcp(1);
            break;
        
        case Bytecodes.LDC_W:                     // 0x13;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.LDC2_W:                    // 0x14;
            VM.increment_bcp(2);
            break;
        
        /*============================================================================*/

        case Bytecodes.ILOAD:                     // 0x15;
            VM.increment_bcp(1);
            break;
        
        case Bytecodes.LLOAD:                     // 0x16;
            VM.increment_bcp(1);
            break;
        
        case Bytecodes.FLOAD:                     // 0x17;
            VM.increment_bcp(1);
            break;

        case Bytecodes.DLOAD:                     // 0x18;
            VM.increment_bcp(1);
            break;
        
        case Bytecodes.ALOAD:                     // 0x19;
            VM.increment_bcp(1);
            break;
        
        /*============================================================================*/

        case Bytecodes.ILOAD_0:                   // 0x1A;
            break;
        
        case Bytecodes.ILOAD_1:                   // 0x1B;
            break;
        
        case Bytecodes.ILOAD_2:                   // 0x1C;
            break;
        
        case Bytecodes.ILOAD_3:                   // 0x1D;
            break;
        
        /*============================================================================*/

        case Bytecodes.LLOAD_0:                   // 0x1E;
            break;
        
        case Bytecodes.LLOAD_1:                   // 0x1F;
            break;

        case Bytecodes.LLOAD_2:                   // 0x20;
            break;
        
        case Bytecodes.LLOAD_3:                   // 0x21;
            break;
        
        /*============================================================================*/

        case Bytecodes.FLOAD_0:                   // 0x22;
            break;
        
        case Bytecodes.FLOAD_1:                   // 0x23;
            break;
        
        case Bytecodes.FLOAD_2:                   // 0x24;
            break;
        
        case Bytecodes.FLOAD_3:                   // 0x25;
            break;
        
        /*============================================================================*/

        case Bytecodes.DLOAD_0:                   // 0x26;
            break;
        
        case Bytecodes.DLOAD_1:                   // 0x27;
            break;

        case Bytecodes.DLOAD_2:                   // 0x28;
            break;
        
        case Bytecodes.DLOAD_3:                   // 0x29;
            break;
        
        /*============================================================================*/

        case Bytecodes.ALOAD_0:                   // 0x2A;
            break;
        
        case Bytecodes.ALOAD_1:                   // 0x2B;
            break;
        
        case Bytecodes.ALOAD_2:                   // 0x2C;
            break;
        
        case Bytecodes.ALOAD_3:                   // 0x2D;
            break;
        
        /*============================================================================*/

        case Bytecodes.IALOAD:                    // 0x2E;
            break;
        
        case Bytecodes.LALOAD:                    // 0x2F;
            break;

        case Bytecodes.FALOAD:                    // 0x30;
            break;
        
        case Bytecodes.DALOAD:                    // 0x31;
            break;
        
        case Bytecodes.AALOAD:                    // 0x32;
            break;
        
        case Bytecodes.BALOAD:                    // 0x33;
            break;
        
        case Bytecodes.CALOAD:                    // 0x34;
            break;
        
        case Bytecodes.SALOAD:                    // 0x35;
            break;
        
        /*============================================================================*/

        case Bytecodes.ISTORE:                    // 0x36;
            VM.increment_bcp(1);
            break;
        
        case Bytecodes.LSTORE:                    // 0x37;
            VM.increment_bcp(1);
            break;

        case Bytecodes.FSTORE:                    // 0x38;
            VM.increment_bcp(1);
            break;
        
        case Bytecodes.DSTORE:                    // 0x39;
            VM.increment_bcp(1);
            break;
        
        case Bytecodes.ASTORE:                    // 0x3A;
            VM.increment_bcp(1);
            break;
        
        /*============================================================================*/

        case Bytecodes.ISTORE_0:                  // 0x3B;
            break;
        
        case Bytecodes.ISTORE_1:                  // 0x3C;
            break;
        
        case Bytecodes.ISTORE_2:                  // 0x3D;
            break;
        
        case Bytecodes.ISTORE_3:                  // 0x3E;
            break;
        
        /*============================================================================*/

        case Bytecodes.LSTORE_0:                  // 0x3F;
            break;

        case Bytecodes.LSTORE_1:                  // 0x40;
            break;
        
        case Bytecodes.LSTORE_2:                  // 0x41;
            break;
        
        case Bytecodes.LSTORE_3:                  // 0x42;
            break;
        
        /*============================================================================*/

        case Bytecodes.FSTORE_0:                  // 0x43;
            break;
        
        case Bytecodes.FSTORE_1:                  // 0x44;
            break;
        
        case Bytecodes.FSTORE_2:                  // 0x45;
            break;
        
        case Bytecodes.FSTORE_3:                  // 0x46;
            break;
        
        /*============================================================================*/

        case Bytecodes.DSTORE_0:                  // 0x47;
            break;

        case Bytecodes.DSTORE_1:                  // 0x48;
            break;
        
        case Bytecodes.DSTORE_2:                  // 0x49;
            break;
        
        case Bytecodes.DSTORE_3:                  // 0x4A;
            break;
        
        /*============================================================================*/

        case Bytecodes.ASTORE_0:                  // 0x4B;
            break;
        
        case Bytecodes.ASTORE_1:                  // 0x4C;
            break;
        
        case Bytecodes.ASTORE_2:                  // 0x4D;
            break;
        
        case Bytecodes.ASTORE_3:                  // 0x4E;
            break;
        
        /*============================================================================*/

        case Bytecodes.IASTORE:                   // 0x4F;
            break;

        case Bytecodes.LASTORE:                   // 0x50;
            break;
        
        case Bytecodes.FASTORE:                   // 0x51;
            break;
        
        case Bytecodes.DASTORE:                   // 0x52;
            break;
        
        case Bytecodes.AASTORE:                   // 0x53;
            break;
        
        case Bytecodes.BASTORE:                   // 0x54;
            break;
        
        case Bytecodes.CASTORE:                   // 0x55;
            break;
        
        case Bytecodes.SASTORE:                   // 0x56;
            break;
        
        /*============================================================================*/

        case Bytecodes.POP:                       // 0x57;
            break;

        case Bytecodes.POP2:                      // 0x58;
            break;
        
        case Bytecodes.DUP:                       // 0x59;
            break;
        
        case Bytecodes.DUP_X1:                    // 0x5A;
            break;
        
        case Bytecodes.DUP_X2:                    // 0x5B;
            break;
        
        case Bytecodes.DUP2:                      // 0x5C;
            break;
        
        case Bytecodes.DUP2_X1:                   // 0x5D;
            break;
        
        case Bytecodes.DUP2_X2:                   // 0x5E;
            break;
        
        case Bytecodes.SWAP:                      // 0x5F;
            break;

        /*============================================================================*/

        case Bytecodes.IADD:                      // 0x60;
            break;
        
        case Bytecodes.LADD:                      // 0x61;
            break;
        
        case Bytecodes.FADD:                      // 0x62;
            break;
        
        case Bytecodes.DADD:                      // 0x63;
            break;
        
        /*============================================================================*/

        case Bytecodes.ISUB:                      // 0x64;
            break;
        
        case Bytecodes.LSUB:                      // 0x65;
            break;
        
        case Bytecodes.FSUB:                      // 0x66;
            break;
        
        case Bytecodes.DSUB:                      // 0x67;
            break;
        
        /*============================================================================*/

        case Bytecodes.IMUL:                      // 0x68;
            break;
        
        case Bytecodes.LMUL:                      // 0x69;
            break;
        
        case Bytecodes.FMUL:                      // 0x6A;
            break;
        
        case Bytecodes.DMUL:                      // 0x6B;
            break;
        
        /*============================================================================*/

        case Bytecodes.IDIV:                      // 0x6C;
            break;
        
        case Bytecodes.LDIV:                      // 0x6D;
            break;
        
        case Bytecodes.FDIV:                      // 0x6E;
            break;
        
        case Bytecodes.DDIV:                      // 0x6F;
            break;

        /*============================================================================*/

        case Bytecodes.IREM:                      // 0x70;
            break;
        
        case Bytecodes.LREM:                      // 0x71;
            break;
        
        case Bytecodes.FREM:                      // 0x72;
            break;
        
        case Bytecodes.DREM:                      // 0x73;
            break;
        
        /*============================================================================*/

        case Bytecodes.INEG:                      // 0x74;
            break;
        
        case Bytecodes.LNEG:                      // 0x75;
            break;
        
        case Bytecodes.FNEG:                      // 0x76;
            break;
        
        case Bytecodes.DNEG:                      // 0x77;
            break;

        /*============================================================================*/

        case Bytecodes.ISHL:                      // 0x78;
            break;
        
        case Bytecodes.LSHL:                      // 0x79;
            break;
        
        case Bytecodes.ISHR:                      // 0x7A;
            break;
        
        case Bytecodes.LSHR:                      // 0x7B;
            break;
        
        case Bytecodes.IUSHR:                     // 0x7C;
            break;
        
        case Bytecodes.LUSHR:                     // 0x7D;
            break;
        
        /*============================================================================*/

        case Bytecodes.IAND:                      // 0x7E;
            break;
        
        case Bytecodes.LAND:                      // 0x7F;
            break;

        case Bytecodes.IOR:                       // 0x80;
            break;
        
        case Bytecodes.LOR:                       // 0x81;
            break;
        
        case Bytecodes.IXOR:                      // 0x82;
            break;
        
        case Bytecodes.LXOR:                      // 0x83;
            break;
        
        /*============================================================================*/

        case Bytecodes.IINC:                      // 0x84;
            VM.increment_bcp(2);
            break;
        
        /*============================================================================*/

        case Bytecodes.I2L:                       // 0x85;
            break;
        
        case Bytecodes.I2F:                       // 0x86;
            break;
        
        case Bytecodes.I2D:                       // 0x87;
            break;

        case Bytecodes.L2I:                       // 0x88;
            break;
        
        case Bytecodes.L2F:                       // 0x89;
            break;
        
        case Bytecodes.L2D:                       // 0x8A;
            break;
        
        case Bytecodes.F2I:                       // 0x8B;
            break;
        
        case Bytecodes.F2L:                       // 0x8C;
            break;
        
        case Bytecodes.F2D:                       // 0x8D;
            break;
        
        case Bytecodes.D2I:                       // 0x8E;
            break;
        
        case Bytecodes.D2L:                       // 0x8F;
            break;

        case Bytecodes.D2F:                       // 0x90;
            break;
        
        case Bytecodes.I2B:                       // 0x91;
            break;
        
        case Bytecodes.I2C:                       // 0x92;
            break;
        
        case Bytecodes.I2S:                       // 0x93;
            break;
        
        /*============================================================================*/

        case Bytecodes.LCMP:                      // 0x94;
            break;
        
        case Bytecodes.FCMPL:                     // 0x95;
            break;
        
        case Bytecodes.FCMPG:                     // 0x96;
            break;
        
        case Bytecodes.DCMPL:                     // 0x97;
            break;

        case Bytecodes.DCMPG:                     // 0x98;
            break;
        
        /*============================================================================*/

        case Bytecodes.IFEQ:                      // 0x99;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.IFNE:                      // 0x9A;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.IFLT:                      // 0x9B;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.IFGE:                      // 0x9C;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.IFGT:                      // 0x9D;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.IFLE:                      // 0x9E;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.IF_ICMPEQ:                 // 0x9F;
            VM.increment_bcp(2);
            break;

        case Bytecodes.IF_ICMPNE:                 // 0xA0;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.IF_ICMPLT:                 // 0xA1;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.IF_ICMPGE:                 // 0xA2;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.IF_ICMPGT:                 // 0xA3;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.IF_ICMPLE:                 // 0xA4;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.IF_ACMPEQ:                 // 0xA5;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.IF_ACMPNE:                 // 0xA6;
            VM.increment_bcp(2);
            break;
        
        /*============================================================================*/

        case Bytecodes.GOTO:                      // 0xA7;
            VM.increment_bcp(2);
            break;

        case Bytecodes.JSR:                       // 0xA8;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.RET:                       // 0xA9;
            VM.increment_bcp(1);
            break;
        
        /*============================================================================*/

        case Bytecodes.TABLESWITCH:               // 0xAA;
            break;

        case Bytecodes.LOOKUPSWITCH:              // 0xAB;
            break;
        
        /*============================================================================*/

        case Bytecodes.IRETURN:                   // 0xAC;
            System.exit(0);
            break;
        
        case Bytecodes.LRETURN:                   // 0xAD;
            System.exit(0);
            break;
        
        case Bytecodes.FRETURN:                   // 0xAE;
            System.exit(0);
            break;
        
        case Bytecodes.DRETURN:                   // 0xAF;
            System.exit(0);
            break;

        case Bytecodes.ARETURN:                   // 0xB0;
            System.exit(0);
            break;
        
        case Bytecodes.RETURN:                    // 0xB1;
            System.exit(0);
            break;
        
        /*============================================================================*/

        case Bytecodes.GETSTATIC:                 // 0xB2;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.PUTSTATIC:                 // 0xB3;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.GETFIELD:                  // 0xB4;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.PUTFIELD:                  // 0xB5;
            VM.increment_bcp(2);
            break;
        
        /*============================================================================*/

        case Bytecodes.INVOKEVIRTUAL:             // 0xB6;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.INVOKESPECIAL:             // 0xB7;
            VM.increment_bcp(2);
            break;

        case Bytecodes.INVOKESTATIC:              // 0xB8;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.INVOKEINTERFACE:           // 0xB9;
            VM.increment_bcp(4);
            break;
        
        /*============================================================================*/

        case Bytecodes.UNUSED:                    // 0xBA;
            break;
        
        /*============================================================================*/

        case Bytecodes.NEW:                       // 0xBB;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.NEWARRAY:                  // 0xBC;
            VM.increment_bcp(1);
            break;
        
        case Bytecodes.ANEWARRAY:                 // 0xBD;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.ARRAYLENGTH:               // 0xBE;
            break;
        
        /*============================================================================*/

        case Bytecodes.ATHROW:                    // 0xBF;
            break;

        case Bytecodes.CHECKCAST:                 // 0xC0;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.INSTANCEOF:                // 0xC1;
            VM.increment_bcp(2);
            break;
        
        /*============================================================================*/

        case Bytecodes.MONITORENTER:              // 0xC2;
            break;
        
        case Bytecodes.MONITOREXIT:               // 0xC3;
            break;
        
        /*============================================================================*/

        case Bytecodes.WIDE:                      // 0xC4;
            break;
        
        /*============================================================================*/

        case Bytecodes.MULTIANEWARRAY:            // 0xC5;
            VM.increment_bcp(3);
            break;
        
        /*============================================================================*/

        case Bytecodes.IFNULL:                    // 0xC6;
            VM.increment_bcp(2);
            break;
        
        case Bytecodes.IFNONNULL:                 // 0xC7;
            VM.increment_bcp(2);
            break;

        /*============================================================================*/

        case Bytecodes.GOTO_W:                    // 0xC8;
            VM.increment_bcp(4);
            break;
        
        case Bytecodes.JSR_W:                     // 0xC9;
            VM.increment_bcp(4);
            break;
        
        /*============================================================================*/

        case Bytecodes.BREAKPOINT:                // 0xCA;
            break;
        
        /*============================================================================*/

        default:
            throw new RuntimeException("Runtime exception: illegal bytecode encountered");
        }

        // Proceed to the next bytecode to be executed
        VM.increment_bcp(1);

    }
  }
}
