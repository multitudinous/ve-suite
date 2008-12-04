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
#include <AspenDynamicsInterface.h>

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

    //std::string currentLevelName;
    //int levelCount;

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

    //void NewParseFile(const char * dynFile);
    //void ReadHeader( std::ifstream inFile );
    //void ReadEncrypted( std::ifstream inFile );
    //void ReadSystemData( std::ifstream inFile );
    //void ReadFlowsheetComponents( std::ifstream inFile );
    //void ReadConstraints( std::ifstream inFile );
    //void ReadGraphicsInformation( std::ifstream inFile );
    //bool PeekFlowsheet( std::ifstream file );
    //void NormalizeForWX();

public:
	DynParser();
	~DynParser();
	void ParseFile( const char * file);
    std::string CreateNetwork( void );	
	void SetWorkingDir( std::string dir );
    void OpenFile( const char * file );
    void CloseFile( );
    void SaveFile( );
    void SaveAs( const char * filename );
    void SetVisibility( bool show );
    void ReinitDynamics();
    std::string GetModuleParams(std::string modname);
    void SetValue( std::string modname, std::string paramname, std::string value );
    AspenDynamicsInterface::AspenDynamicsInterface * dyndoc;
};

#endif