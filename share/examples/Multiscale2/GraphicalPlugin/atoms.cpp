/*
 * Originally code for Pat McCormick
 * Adapted by Christine Ahrens
 * 10/25/10
 */

#include "atoms.h"
#include <limits>
#include <cassert>
#include <iostream>
#include <fstream>
#include <cstdio>
#include <string>
//#include <stdint.h>
#include <math.h>

#include <sys/types.h>
#include <sys/stat.h>
//#include <unistd.h>
//#include <ANN/ANN.h> 

#define ATOM_TIMER 0

#define DEFAULT_RADIUS 1.2f


// if we're in a 8.3 format, avoid numbers that overflow into 9 digits
static float clip(const float x)
{
	return (x > 9999) ? 9999 : x;
}



atoms_t::atoms_t(int maxAtoms) : maxAtoms(maxAtoms)
#if ATOM_TIMER
, sw()
#endif
{
  atoms = new Atom[maxAtoms];
}

void atoms_t::Read(const std::string& filename) 
{
	GetAtomCount(filename);
	ReadAtoms(filename);
	//FindBounds();  // not needed at the moment
}

atoms_t::~atoms_t() 
{
	delete atoms;
}

void atoms_t::GetAtomCount(const std::string& filename)
{
	numAtoms = 0;
#if ATOM_TIMER
	sw.startTimer();
#endif
	struct stat filestatus;

	stat( filename.c_str(), &filestatus );

  numAtoms = filestatus.st_size/sizeof(Atom);

	//printf("Stat num of atoms: %d\n", numAtoms);
#if ATOM_TIMER
	sw.stopTimer();
	printf("CountTime: %f\n", sw.getElapsedTime());
#endif

/*
  FILE *fp = fopen(filename.c_str(), "r");

  if (fp == 0) {
		printf("Can't open file in GetAtomCount!");
    return;
	}

  Atom atom;

	numAtoms = 0;

  while(! feof(fp)) {
    size_t n = fread((void*)&atom, sizeof(Atom), 1, fp);
    if (feof(fp))
      break;
    numAtoms++;
  }

  fclose(fp);
	printf("fread num of atoms: %d\n", numAtoms);
*/
	printf("numAtoms:%d\n",numAtoms);

}

// takes about 5.5 seconds if doing stat read for size
void atoms_t::ReadAtoms(const std::string& filename)
{
#if ATOM_TIMER
	sw.startTimer();
#endif
  FILE *fp = fopen(filename.c_str(), "r");
  if (fp == 0) {
		printf("Can't open file in ReadAtoms!");
    return ;
  }

  size_t n = fread((void*)atoms, sizeof(Atom), numAtoms, fp);
  assert(n == numAtoms);

  fclose(fp);
#if ATOM_TIMER
	sw.stopTimer();
	printf("ReadTime: %f\n", sw.getElapsedTime());
#endif

}

// takes about .42 seconds
void atoms_t::FindBounds() 
{
#if ATOM_TIMER
	sw.startTimer();
#endif
	
	minX = minY = minZ = std::numeric_limits<float>::max();
	maxX = maxY = maxZ = std::numeric_limits<float>::min();

	for(uint i = 0; i < numAtoms; ++i) {

		if (atoms[i].x < minX)
			minX = atoms[i].x;

		if (atoms[i].x > maxX)
			maxX = atoms[i].x;


		if (atoms[i].y < minY)
			minY = atoms[i].y;

		if (atoms[i].y > maxY)
			maxY = atoms[i].y;


		if (atoms[i].z < minZ)
			minZ = atoms[i].z;

		if (atoms[i].z > maxZ)
			maxZ = atoms[i].z;
	}
#if ATOM_TIMER
	sw.stopTimer();
	printf("FindBoundsTime: %f\n", sw.getElapsedTime());
#endif
}


float atoms_t::GetCenterX(uint index) 
{
	return atoms[index].x;
}

float atoms_t::GetCenterY(uint index) 
{
	return atoms[index].y;
}
	
float atoms_t::GetCenterZ(uint index) 
{
	return atoms[index].z;
}

float atoms_t::GetRadius(uint) 
{
	return DEFAULT_RADIUS;
}

float atoms_t::GetCentrosymmetry(uint index) 
{
	return atoms[index].centrosymmetry;
}

float atoms_t::GetBondOrder(uint index) 
{
	return atoms[index].bond_order;
}


float atoms_t::GetMinX()
{
	return minX;
}
 
float atoms_t::GetMaxX()
{
	return maxX;
}
 
float atoms_t::GetMinY()
{
	return minY;
}
 
float atoms_t::GetMaxY()
{
	return maxY;
}
 
float atoms_t::GetMinZ()
{
	return minZ;
}
 
float atoms_t::GetMaxZ()
{
	return maxZ;
}
 
// Returns the number of spheres 
uint atoms_t::Size()
{
  return numAtoms;
}

void atoms_t::writePDB(std::string fn, atoms_t ) 
{
    FILE *fp = fopen(fn.c_str(), "w");

    /*ANNpointArray           dataPts;
    ANNpoint                        queryPt; 

    ANNidxArray                     nnIdx;                                  // near neighbor indices
    ANNdistArray            dists;                                  // near neighbor distances
    ANNkd_tree*                     kdTree;                                 // search structure

    dataPts = annAllocPts(numAtoms, 3);
    queryPt = annAllocPt(3);  
    for(uint i = 0; i < numAtoms; i+=1) {
    dataPts[i][0] = atoms[i].x;
    }
    nnIdx = new ANNidx[1];
    dists = new ANNdist[1];*/
    //kdTree = new ANNkd_tree(dataPts,numAtoms, 3);
    for(uint i = 0; i < numAtoms; i+=1 ) 
    {
        int numfound = 0;
        numfound = 0;
        //if(i%1000 == 0)
        //float distsq = pow(atoms[i].x - otherAtoms.atoms[i].x,2) + pow(atoms[i].y - otherAtoms.atoms[i].y,2) +pow(atoms[i].z - otherAtoms.atoms[i].z,2);
        //printf("%d %f\n",i, distsq );
        /*for(uint j = 0; j < numAtoms; j+=1) {
        float distsq;
        distsq = pow(atoms[i].x-atoms[j].x,2)+pow(atoms[i].y-atoms[j].y,2)+pow(atoms[i].z-atoms[j].z,2);
        if(distsq<16)
        numfound++;
        }*/
        if(atoms[i].centrosymmetry > 1) 
        {
            int serial = 1;
            std::string name = "C";
            char altLoc = ' ';
            std::string resName = "C";
            char chainID ='A';
            int resSeq = 1;
            char iCode = ' ';
            float x = atoms[i].x;
            float y = atoms[i].y;
            float z = atoms[i].z;
            float occ = 1;
            float temp = 1;
            std::string element = "C";
            std::string charge = "1";
            if(1) 
            {
                fprintf(fp,"HETATM");
                fprintf(fp,"%5d ",serial); // note trailing space
                fprintf(fp,"%-4s",name.c_str());
                fprintf(fp,"%c",altLoc);
                fprintf(fp,"%-3s",resName.c_str());
                fprintf(fp," %c",chainID);
                fprintf(fp,"%4d",resSeq);
                fprintf(fp,"%c   ",iCode);      // note 3 spaces
                fprintf(fp,"%8.3f",clip(x));
                fprintf(fp,"%8.3f",clip(y));
                fprintf(fp,"%8.3f",clip(z));
                fprintf(fp,"%6.2f",occ);
                fprintf(fp,"%6.2f",temp);
                fprintf(fp,"          ");       // 10 blank spaces
                fprintf(fp,"%-2s",element.c_str());
                fprintf(fp,"%-2s",charge.c_str());
                fprintf(fp,"\n");
            }
        }
    }
    fclose(fp);
}

