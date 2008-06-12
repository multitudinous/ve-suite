#ifndef STOVE_DATA_H
#define STOVE_DATA_H

//#include <stdlib.h>
#include <stdio.h>
#include <vector>
#include "stove.h"
//#include "../../Control/Control.h"

	
/**************************************************************************/
// used in simulation mode
class StoveData
{
public:
	StoveData( Control *);
	virtual ~StoveData();

	typedef std::vector<Stove *> StoveList;
	StoveList s_initpop, s_me;

	void OutputSimulationData( void );
	Stove *GetStoveFromInitialPopulation( int i );
	Stove *GetStoveFromMatingEventNumber( int i );
	int FindNearestStoveFromInitialPopulation( Stove *s2 );
	int FindNearestStoveFromMatingEvents( Stove *s2 );
	void writeData( FILE *file );
};
#endif
