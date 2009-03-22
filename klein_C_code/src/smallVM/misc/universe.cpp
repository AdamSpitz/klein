# include "universe.hh"
# include "generation.hh"
# include "space.hh"

NewGeneration* Universe::newGeneration() {
  return new NewGeneration(contentsOfSlotNamed("newGeneration"));
}

# define GENERATIONS_DO(x, body) NewGeneration* x = newGeneration(); body
# define SPACES_DO(x, body) EdenSpace* x = newGeneration()->edenSpace(); body

BPRef Universe::allocateBytes(int nBytes) {
  return newGeneration()->allocateBytes(nBytes);
}

Address Universe::allocateOops(int nOops) {
  return newGeneration()->allocateOops(nOops);
}

void Universe::verify(Verifier* aVerifier) {
  verifyGenerations         (aVerifier);
  verifyCanonicalizedStrings(aVerifier);
}

void Universe::verifyCanonicalizedStrings(Verifier* aVerifier) {
  // TODO: implement this.
}

void Universe::verifyGenerations(Verifier* aVerifier) {
  GENERATIONS_DO(g,
    g->verify(aVerifier);
  )
}

bool Universe::isBytesPartAddressInBounds(Address bpAddr) {
  SPACES_DO(s,
    if ( s->isBytesPartAddressInBounds(bpAddr) ) return true;
  )
  return false;
}

bool Universe::isObjectAddressInBounds(Address addr) {
  SPACES_DO(s,
    if ( s->isObjectAddressInBounds(addr) ) return true;
  )
  return false;
}
