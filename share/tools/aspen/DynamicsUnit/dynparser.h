#include <fstream>
#include <iostream>
#include <string>
#include <utility>
#include <sstream>
#include <map>
#include <vector>
#include "AspenPlusLUT.h"
#include "AspenIconData.h"

class DynParser
{

private:
	std::vector<float> xCoords;
	std::vector<float> yCoords;
    std::map< std::string, std::map< std::string, std::pair< float, float > > > iconLocations;
	
	typedef struct
	{
		std::string id;
		std::string type;
		std::string icon;
		float scale;
		float rotation;
		int mirror;
		bool hierarchical;
        bool iconHidden;
	}BlockInfo;

	std::map< std::string, std::map< std::string, BlockInfo > > BlockInfoList;

public:
	DynParser();
	~DynParser();
	void ParseFile(const char *);	
};
