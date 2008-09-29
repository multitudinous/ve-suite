#ifndef DYNPARSER_H
#define DYNPARSER_H

#include <fstream>
#include <iostream>
#include <sstream>
#include <map>
#include <vector>
#include <string>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/System.h>
#include <AspenPlusInterface.h>

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

    typedef struct
	{	
		std::string streamId;
		int streamType;
		std::vector< std::pair< float, float > > value;	
	}streamXY;

	streamXY xy;
	streamXY tempXY;

    std::vector< streamXY > streamCoordList;
    std::map< std::string, std::map< std::string, std::vector< std::pair< float, float > > > > linkPoints;
    std::map< std::string, std::map< std::string, int > > linkTypes;
    std::map< std::string, std::map< std::string, std::string > > inLinkToModel;
    std::map< std::string, std::map< std::string, std::string > > outLinkToModel;
    std::map<std::string, std::map< std::string, int > >models;
    std::map< std::string, std::pair< int, int > > streamPortIDS;
    int redundantID;
    std::string workingDir;
    void CreateNetworkLinks( ves::open::xml::model::NetworkPtr subNetwork, std::string hierName );
    void ParseSubSystem(ves::open::xml::model::ModelPtr model, std::string networkName);

public:
	DynParser();
	~DynParser();
	void ParseFile(const char *);
    std::string CreateNetwork( void );	
	void SetWorkingDir( std::string dir );
    void OpenFile(const char * file);
    void CloseFile( );
    void SaveFile( );
    void SaveAs(const char * filename);
    AspenPlusInterface::AspenPlusInterface * dyndoc;
};

#endif