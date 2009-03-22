package com.sun.kanban.character_sets;

import com.sun.kanban.utilities.DebugTools;


// used to test if a charcter is in a given class

public class CharacterSetInclusion implements CharacterSetClosure {
    protected boolean _table[];
    
    public CharacterSetInclusion(CharacterSet cc) { 
        _table = new boolean[CharacterSet.max_character_value + 1]; 
        cc.Do(this); 
    }
    
    
    public void for_char_in_class(char c) {
        if (_table[c])
            throw DebugTools.new_error("Duplicate character: " + c);
        _table[c] = true;
    }
    
    public String toString() {
        String s = super.toString() + " including: \"";
        for (char c = 0; c < _table.length; ++c)
            if (includes(c))
                s = s + c;
        return s + "\"";
    }
    
    public boolean includes(char c) { return _table[c]; }
}
