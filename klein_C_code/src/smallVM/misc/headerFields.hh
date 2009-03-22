# ifndef KLEIN_HEADER_FIELDS_H
# define KLEIN_HEADER_FIELDS_H

# include "base.hh"

class Layout;
class MemoryObjectLayout;
class NumberBitField;
class Verifier;

class AbstractHeaderField {
  protected:
	NumberBitField*      _markField;
	AbstractHeaderField* _precedingField;
  public:
	AbstractHeaderField( NumberBitField* mf, AbstractHeaderField* pf ) : _markField(mf), _precedingField(pf) {}
	
	NumberBitField* markField() { return _markField; }
	
	bool isFirst() { return _precedingField == NULL; }
	
	bool isIncludedIn   (Oop o, MemoryObjectLayout* aLayout);
	int  indexFor       (Oop o, MemoryObjectLayout* aLayout);
	int  indexAfterMeFor(Oop o, MemoryObjectLayout* aLayout);
};

// TODO: It's kinda ridiculous that I've duplicated so much stuff between the fields with various kinds of values.
// I've been worried about implicit conversions and stuff, so I've probably been overly cautious. Maybe this
// duplication can be collapsed. -- Adam 11/05

class OopValueHeaderField : public AbstractHeaderField {
 public:
  OopValueHeaderField(NumberBitField* mf, AbstractHeaderField* pf) : AbstractHeaderField(mf, pf) {}
  
  Oop     valueFor(Oop o,        MemoryObjectLayout* aLayout);
  void setValueFor(Oop o, Oop v, MemoryObjectLayout* aLayout);
    
  virtual Oop valueForEncodedNumberInMark(int n);
};

class MapHeaderField : public AbstractHeaderField {
 public:
  virtual Oop valueForEncodedNumberInMark(int n);
};

class SmiValueHeaderField : public AbstractHeaderField {
 public:
  SmiValueHeaderField(NumberBitField* mf, AbstractHeaderField* pf) : AbstractHeaderField(mf, pf) {}
  int     valueFor(Oop o,        MemoryObjectLayout* aLayout);
  void setValueFor(Oop o, int v, MemoryObjectLayout* aLayout);

  virtual int valueForEncodedNumberInMark(int n)  { return n; }
  
  int verifyObject(Oop o, MemoryObjectLayout* aLayout, Verifier* aVerifier);
};

class MarkValueHeaderField : public AbstractHeaderField {
 public:
  MarkValueHeaderField(NumberBitField* mf, AbstractHeaderField* pf) : AbstractHeaderField(mf, pf) {}
  MarkValue    valueFor(Oop o,              MemoryObjectLayout* aLayout);
  void      setValueFor(Oop o, MarkValue v, MemoryObjectLayout* aLayout);
  
  virtual MarkValue valueForEncodedNumberInMark(int n) {
    error("should never be called - we can't encode the mark in the mark :)");
    return 0;
  }
};

# endif // KLEIN_HEADER_FIELDS_H
