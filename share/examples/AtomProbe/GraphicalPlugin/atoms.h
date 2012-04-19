/*
 *  Originally code from Pat McCormick
 *  Adapted by Christine Ahrens
 *  10/25/10
 */

#ifndef atoms_h
#define atoms_h

//#include <optixu/optixpp.h>

#include <vector>
#include <string>
//#include "hr_time.h"
//#define uint unsigned int

typedef unsigned int uint;

struct Atom {
  float  x, y, z;       
  float  bond_order;
  float  centrosymmetry;
};

class atoms_t {
  public:
		atoms_t(int maxAtoms);
    ~atoms_t();
		void cleanUp() { if (atoms) {delete atoms; atoms = NULL;} }
    void Read(const std::string& filename);
    uint Size();
    float GetCenterX(uint index);
    float GetCenterY(uint index);
    float GetCenterZ(uint index);
    float GetRadius(uint index);
    float GetCentrosymmetry(uint index);
    float GetBondOrder(uint index);
		float GetMinX();
		float GetMaxX();
		float GetMinY();
		float GetMaxY();
		float GetMinZ();
		float GetMaxZ();
	void writePDB(std::string fn,atoms_t otherAtoms);
  private:
    float minX, maxX, minY, maxY, minZ, maxZ;
    uint numAtoms, maxAtoms;
    Atom* atoms; // atom info
	//	CStopWatch sw;

    void GetAtomCount(const std::string& filename);
    void ReadAtoms(const std::string& filename);
    void FindBounds();
	
};
 
#endif
