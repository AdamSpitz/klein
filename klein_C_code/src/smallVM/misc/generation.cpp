# include "generation.hh"
# include "space.hh"

EdenSpace* NewGeneration::edenSpace() {
  return new EdenSpace(contentsOfSlotNamed("edenSpace"));
}


BPRef NewGeneration::allocateBytes(int nBytes) {
  return edenSpace()->allocateBytes(nBytes);
}

Address NewGeneration::allocateOops(int nOops) {
  return edenSpace()->allocateOops(nOops);
}

# define SPACES_DO_WITH_NAME(x, xName, body) {Space* x = edenSpace(); char* xName = "eden"; body}

void NewGeneration::verify(Verifier* aVerifier) {
  SPACES_DO_WITH_NAME(s, sName,
    s->verifySpace(sName, aVerifier);
  )
}
