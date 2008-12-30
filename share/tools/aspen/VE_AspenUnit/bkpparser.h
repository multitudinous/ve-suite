//
//  BKPParser - parses Aspen bkp files to acquire information about Graphics
//  Terry E. Jordan Jr.  - SAIC
//  NETL/DOE
//

#ifndef BKPPARSER_H
#define BKPPARSER_H

#include <string>
#include <vector>
#include <map>
#include <utility>
#include <sstream>
#include <CASI.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/System.h>

namespace ves
{
namespace open
{
namespace xml
{
namespace model
{
   class Network;
}
}
}
}

class BKPParser
{

private:
	void ParseFile(const char *);                                            //parses the bkp file
	std::vector<float> xCoords;
	std::vector<float> yCoords;	

	typedef struct                                                     //struct with blocks type and id
	{
		std::string id;
		std::string type;
		std::string icon;
        float width;
        float height;
		float scale;
		float rotation;
		int mirror;
		bool hierarchical;
        bool iconHidden;
	}BlockInfo;

	//std::map< std::string, BlockInfo> BlockInfoList;
	std::map< std::string, std::map< std::string, BlockInfo > > BlockInfoList;
	
	typedef struct                                                     //struct that contain the stream id, type & coordinates
	{	
		std::string streamId;
		int streamType;
		std::vector< std::pair< float, float > > value;	
	}streamXY;

	streamXY xy;
	streamXY tempXY;
	std::vector< streamXY > streamCoordList;	                           //coordinate list of a given stream

   // link name, model name
   //std::map< std::string, std::string > inLinkToModel;
   //std::map< std::string, std::string > outLinkToModel;
   std::map< std::string, std::map< std::string, std::string > > inLinkToModel;
   std::map< std::string, std::map< std::string, std::string > > outLinkToModel;
   //container to hold link points with stream name
   //std::map< std::string, std::vector< std::pair< float, float > > > linkPoints;
   std::map< std::string, std::map< std::string, std::vector< std::pair< float, float > > > > linkPoints;
   std::map< std::string, std::map< std::string, int > > linkTypes;
   // model name with number
   //std::map< std::string, int > models;
   std::map<std::string, std::map< std::string, int > >models;
   // model name with icon location
   //std::map< std::string, std::pair< float, float > > iconLocations;
   std::map< std::string, std::map< std::string, std::pair< float, float > > > iconLocations;
   // stream name to port ids from and to
   std::map< std::string, std::map< std::string, std::pair< int, int > > > streamPortIDS;

   //VE_XML::VE_Model::Network* veNetwork;
   void StripCharacters( std::string& data, std::string character );   

   std::string workingDir;
   int redundantID;

public:
	CASI::CASIDocument * aspendoc;

	BKPParser();                                                       //constructor
	~BKPParser();                                                      //deconstrutor
	void openFile(const char *);                                             //opens the given file
	void closeFile();                                             //close the file
	void saveFile();  
	void saveAs(const char *);                
	void showAspen(bool);    
	void ReinitAspen();
    void ReinitBlock( std::string modname );
	void step();
	int getNumComponents();                                            //returns total components
	//std::string getBlockType(std::string);                                     //returns the filename of component
	std::string getBlockType(std::string blockName, std::string flowsheetName = NULL); //returns the filename of component
	//std::string getBlockID(std::string);                                       //returns the filename of component
	std::string getBlockID(std::string blockName, std::string flowsheetName = NULL);                                      //returns the filename of component
	float getXCoord(int);                                              //returns the x coordinates of component
	float getYCoord(int);                                              //returns the y coordinates of component
	float getStreamXCoord(int streamIndex, int coordIndex); //returns X coord of one point of stream
	float getStreamYCoord(int streamIndex, int coordIndex); //returns y coord of one point of stream
	std::string getStreamId(int);                           //returns the stream's id
	int getStreamType(int);                                 //returns the stream's type
	int getNumStream();                                     //returns total number of the streams
	int getStreamSize(int index);                           //returns the total number of points for a stream
	bool isOpen();

	void SetWorkingDir( std::string dir );

	void CreateNetworkLinks( ves::open::xml::model::NetworkPtr subNetwork, std::string hierName );
   void CreateNetworkInformation( std::string networkData );
   void ParseSubSystem(ves::open::xml::model::ModelPtr model, std::string networkName);
   std::string CreateNetwork( void );
   std::string GetInputModuleParamProperties(std::string modname, std::string paramName);
   std::string GetInputModuleParams(std::string modname);
   std::string GetOutputModuleParamProperties(std::string modname, std::string paramName);
   std::string GetOutputModuleParams(std::string modname);
   std::string GetStreamInputModuleParamProperties(std::string modname, std::string paramName);
   std::string GetStreamInputModuleParams(std::string modname);
   std::string GetStreamOutputModuleParamProperties(std::string modname, std::string paramName);
   std::string GetStreamOutputModuleParams(std::string modname);
};

#endif
