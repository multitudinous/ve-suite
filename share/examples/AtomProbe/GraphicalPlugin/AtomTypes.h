#pragma once
#include <gmtl/Vec.h>
#include <string>
#include <vector>

#include <osg/Geode>
#include <osg/Geometry>


#include <ves/util/SimpleDataTypeSignalSignatures.h>

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/SignalWrapper.h>


typedef boost::signals2::signal< void ( const std::vector< int >&, const std::vector< int >&, const std::vector< int >&, const std::vector< double >&  ) > ColorDoubleVectorSignal_type;

typedef boost::signals2::signal< void (  const std::vector< double >&  ) > DoubleVectorSignal_type;


void SwapBytes(void *pv, size_t n);
#define SWAP(x) SwapBytes(&x, sizeof(x));
//This file defines the raw data types used in Atom vis and analyis

class Atoms;

//Atom Filter interface
class AtomFilter {
public:
	std::string filterType;
	virtual void applyFilter() = 0;
	
	
};

//Atom Colorization interface
class AtomColorizer {
public:
	//int fieldIndex;
	//bool useRelativeRange;
	//bool usePercentageClip;
	//float topClip, bottomClip;
	std::string colorizationType;
	//Atoms *_atoms;
	virtual void colorize(osg::Vec4ubArray* colours) = 0;
};


//masks the atoms based on the selected field
template<class T>
class RampMask : AtomFilter {
public:
	int fieldIndex;
	bool useRelativeRange;
	bool usePercentageClip;
	float topClip, bottomClip;
	void applyFilter();
	std::vector<double> histogram;
	T* _rawData;
	unsigned int rowWidth;
	DoubleVectorSignal_type m_MaskHistogramDataSignal;
	
	//construct a new ramp mask filter given a set of atoms and the field
	RampMask<T>(Atoms* atoms, unsigned int field);	
	
	Atoms *_atoms;
};


//masks and colorizes the atoms based on the selected field
template<class T>
class RampDiscrete : public AtomFilter, public AtomColorizer {
public:
	int fieldIndex;
	bool useRelativeRange;
	std::vector<double> rangeBottoms;
	std::vector<double> rangeTops;
	std::vector<int> r, g, b;
	std::vector<bool> enabled;
	
	void applyFilter();
	std::vector<double> histogram;
	T* _rawData;
	unsigned int rowWidth;
	
	DoubleVectorSignal_type m_MaskHistogramDataSignal;
	Atoms *_atoms;
	//construct a new ramp discrete filter given a set of atoms and the field
	RampDiscrete<T>(Atoms* atoms, unsigned int field);
	
};



template<class T>
class RampColorizer : AtomColorizer {
	int fieldIndex;
	bool useRelativeRange;
	std::vector<double> rangeBoundaries;
	std::vector<int> r, g, b;
	
	float topClip, bottomClip;
	void applyFilter();
	std::vector<double> histogram;
	T* _rawData;
	unsigned int rowWidth;
	
	DoubleVectorSignal_type m_RampHistogramDataSignal;
	Atoms *_atoms;
	
	//construct a new ramp colorizer
	RampColorizer<T>(Atoms* atoms, unsigned int field);
};

class Atoms {
public:
	enum FieldType {
		IntRamp,
		IntDiscrete,
		FloatRamp,
		FloatDiscrete,
		DoubleRamp,
		DoubleDiscrete,
	};
	struct Field {
		FieldType fieldType;
		int offset;
		std::string name;
		//this is in bytes
		unsigned int fieldWidth;
	};
	
	
	
	//this part is experimental, not sure this is the best way to deal with this
	std::vector<void *> extraChannels;
	std::vector<Field> extraChannelFields;
	//std::vector<
	
	void loadEPOS(const char* fn);
	void prepareAtomsForRendering(osg::Vec3Array* vertices, osg::Vec4ubArray* colours);
	void reinitializeCurrentAtoms();
	void zeroExtraChannels();
	void reflowFilters();
	
	void * getRawData() {
		return _rawData;
	}
	unsigned int getNumAtoms() {
		return _numAtoms;
	}
	
private:
	std::vector<gmtl::Vec3f> _positions;
	void * _rawData;
	unsigned int _numAtoms;
	int _rowWidth;
	std::vector<Field> _fields;
	
	std::vector<int> _currentAtoms;
	
	
	//friend classes that are tightly integrated and need access to private data variables
	friend class AtomFilter;
	template<class T> friend class RampMask;
	
	//this friend relationship is needed so the time series can ensure that filters/colorization is the same across all
	friend class AtomTimeSeries;
	
};



class AtomsTimeSeries {
public:
	std::vector<Atoms> frames;
	void loadCustomXYZ(char* firstFN);
	
};