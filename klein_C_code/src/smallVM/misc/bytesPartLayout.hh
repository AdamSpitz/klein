# ifndef KLEIN_BYTES_PART_LAYOUT_H
# define KLEIN_BYTES_PART_LAYOUT_H

# include "layout.hh"

class BytesPartLayout : public AbstractLayout {
  public:
	BPRef allocateBytesPartWithIndexableSize(int nBytes);

	Byte* addressOfFirstByteInBytesPart(BPRef bpRef);

	int      indexableSizeOfBytesPart(BPRef bpRef       );
	void set_indexableSizeOfBytesPart(BPRef bpRef, int s);

	Byte* forBytesPart_AddressAt(BPRef bpRef, int i        );
	Byte  forBytesPart_At       (BPRef bpRef, int i        );
	void  forBytesPart_At_Put   (BPRef bpRef, int i, Byte b);
	
	BPRef nextBytesPartAfter(BPRef bpRef);
};

# endif // KLEIN_BYTES_PART_LAYOUT_H
