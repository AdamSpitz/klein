package com.sun.kanban.parser_framework; 

import java.io.*;

import com.sun.kanban.parser_framework.lexer.input_streams.*;

interface bazorp extends apple, orange {}

abstract public class ParserTest extends foo.bar implements foo, a.b {
  static {foo = 12;}

  ParserTest(int a, Foo[][][] b) {foo = bar;}

  ParserTest a() {foo = bar;}
 int x = 12;
 Blort foo() {}
 ParserTest() {snort();}

 static { snort = 5; }

	
	abstract public void test(LexerInputStream s, boolean _print );
	
	abstract protected FilenameFilter filter();
	
	public void test_string(String s) {
		System.out.println("Input is: " + s);
		test((new StringLexerInputStream(s)), true);
	}

	public void test_file(String path) {
		System.out.println("Input is file: " + path);
		test((new FileLexerInputStream(path)), true);
	}
	
	public void test_directory(String dir_path) {
		File d = new File(dir_path);
		String names[] = d.list(filter());
		for (int i = 0;  i < names.length;  ++i) {
			test((new FileLexerInputStream(d.getAbsolutePath() + "/" + names[i])), false);
		}
		System.out.println("Done with " + names.length + " files.");
	}
}
