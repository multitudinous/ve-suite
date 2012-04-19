#include "AtomTypes.h"
#include <cstdio>
#include <string>
//#include <stdint.h>
#include <math.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <iostream>


void SwapBytes(void *pv, size_t n)
{
    char *p = (char*)pv;
    size_t lo, hi;
    for(lo=0, hi=n-1; hi>lo; lo++, hi--)
    {
        char tmp=p[lo];
        p[lo] = p[hi];
        p[hi] = tmp;
    }
}



void Atoms::loadEPOS(const char* fn) {
	//fn = "/Volumes/Data/R12_05273-v01.epos";
	//fn = "/Volumes/Data/FeNi Perpendicular-v02.epos";
	struct stat filestatus;
	
	stat( fn, &filestatus );
	std::cout <<fn << std::endl;
    
	_numAtoms = filestatus.st_size/(11*sizeof(float));
	//numAtoms = numAtoms;
	float* buffer, *buffer2;
	
	std::cout << _numAtoms << std::endl;
	buffer = new float[_numAtoms*11*5];
	buffer2 = new float[_numAtoms*11*5];
	FILE* fp;
	
	
	
	fp = fopen(fn, "rb");
	fread(buffer, _numAtoms, sizeof(float)*11, fp);
	_rawData = buffer;
	int* intbuffer = ( int*)buffer;
	for(uint i = 0; i < _numAtoms; i++) {
		for(int ii = 0; ii <11; ii++) {
			SWAP(buffer[i*11+ii]);
		}
		for(int ii = 9; ii < 11; ii++) {
			buffer[i*11+ii] = (float)intbuffer[i*11+ii];
			
		}
		
		
		/*pos.x() = buffer[i*11];
		pos.y() = buffer[i*11+1];
		pos.z() = buffer[i*11+2];
		vertices->push_back(pos);
		
		int h, s, v;
		float height = buffer2[i*4+1];
		height = height*255;
		height+=128;
		h = (int)height;
		
		
		if (h > 255) h = 255;
		if (h < 0) h = 0;
		s = 128;
		v = 256;
		
		HSVtoRGB( &r, &g,&b, h, s, v );
		colours2->push_back(osg::Vec4ub(r,g,b,a));
		 */
	}
	
	//delete buffer;
	fclose(fp);
	
	
	//atomProbeGeometry = geometry;
	
	

}


void Atoms::prepareAtomsForRendering(osg::Vec3Array* vertices, osg::Vec4ubArray* colours) {
	vertices->clear();
	colours->clear();
	//prepareVertices(vertices)
	//colorize(colours);
	

}

void Atoms::reinitializeCurrentAtoms() {
	_currentAtoms.clear();
	for(unsigned int i = 0; i < _numAtoms; i++) {
		_currentAtoms.push_back(i);
	}
}


//this is necessary prior to a full reflow of the filter pipeline
void Atoms::zeroExtraChannels() {
}


//this applies the current filter to the atoms
//assumptions:
//_rawData points to the first instance of the variable we are filtering on in the raw data
//rowWidth is the width of the row in the size of this data type (usually 4 or 8 bytes)
//the max and dividing operations that we are performing here are applicable to T
//m_MaskHistogramDataSignal has been initialized
//
//postconditions
//_atoms->_currentAtoms has been masked according to the mask parameters
template<class T> void RampMask<T>::applyFilter() {
	std::vector<unsigned int> atomsTemp;
	histogram.clear();
	float max = -std::numeric_limits<float>::infinity();
	float min = std::numeric_limits<float>::infinity();
	for(unsigned int i = 0; i < _atoms->_currentAtoms.size(); i++) {
		unsigned int index;
		index = _atoms->_currentAtoms[i]*rowWidth;
		if(_rawData[index] > max)
			max = _rawData[index];
		if(_rawData[index] < min)
			min = _rawData[index];
	}
	
	
	double range = max - min;
	histogram.clear();
	for(int i = 0; i < 40; i++)
		histogram.push_back(0);
	
	for(unsigned int i = 0; i < _atoms->_currentAtoms.size(); i++) {
		unsigned int index;
		index = _atoms->_currentAtoms[i]*rowWidth;
		double percentage = (_rawData[index]-min)/range;
		
		percentage = percentage*40;
		int perint = (int)percentage;
		if(perint > 39)
			perint = 39;
		histogram[perint]+=1;
	}
	for(int i = 0; i < 40; i++) {
		histogram[i] = (histogram[i]/(float)_atoms->_currentAtoms.size()) * 40.;
	}
	
	
	
	float offset = -min;
	float scale = 1./(max - min);
	float clipscale = 1./(topClip - bottomClip);
	//coloursOnCard->clear();
	//vertices->clear();
	int r, g, b,a;
	a = 255;
	for(unsigned int i = 0; i < _atoms->_currentAtoms.size(); i++) {
		unsigned int index;
		index = _atoms->_currentAtoms[i]*rowWidth;
		float rawNum = (_rawData[index]+offset)*scale;
		float colorVal = (rawNum -bottomClip)*clipscale;
		if(colorVal < 0) {
			continue;
		}
		if(colorVal > 1) {
			continue;
		}
		//r = colorVal *255;
		//g = 255;
		//b = colorVal *255;
		//osg::Vec3f pos;
		//pos.x() = _atoms->positions[i][0];//rawData[currentAtoms[i]*11];
		//pos.y() = _atoms->positions[i][2];//rawData[currentAtoms[i]*11+1];
		//pos.z() = _atoms->positions[i][1];//rawData[currentAtoms[i]*11+2];
		atomsTemp.push_back(i);
		//vertices->push_back(pos);
		//coloursOnCard->push_back((*colours)[i]);
		//if(i < 100) {
		/*std::cout << i << std::endl;
		 std::cout << "rawData;" << rawData[index] << std::endl;
		 std::cout << "rawnum:" << rawNum << std::endl;
		 std::cout << "colorVal:" << colorVal << std::endl;*/
		//}
		
	}
	//atomProbeGeometry->setColorArray(coloursOnCard);
	//atomProbeGeometry->setVertexArray(vertices);
	//atomProbeGeometry->setPrimitiveSet(0,new osg::DrawArrays(GL_POINTS,0,vertices->size()));
	m_MaskHistogramDataSignal(histogram);
	_atoms->_currentAtoms = atomsTemp;
	
	
}



template<class T> RampMask<T>::RampMask(Atoms* atoms, unsigned int field) {
	useRelativeRange = true;
	usePercentageClip = true;
	topClip=1.0;
	bottomClip = 0.0;
	_atoms = atoms;
	rowWidth = (_atoms->_rowWidth)/4;
	_rawData = _atoms->_rawData + field;
	fieldIndex = field;
}

template<class T> RampColorizer<T>::RampColorizer(Atoms* atoms, unsigned int field) {
	useRelativeRange = true;
	topClip=1.0;
	bottomClip = 0.0;
	_atoms = atoms;
	rowWidth = (_atoms->_rowWidth)/4;
	_rawData = _atoms->_rawData + field;
	fieldIndex = field;
}