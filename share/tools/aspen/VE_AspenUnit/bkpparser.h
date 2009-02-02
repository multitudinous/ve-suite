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
    //parses the bkp file
	void ParseFile(const char *);                                           
	std::vector<float> xCoords;
	std::vector<float> yCoords;	

    //struct with blocks type and id
	typedef struct                                                     
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

	std::map< std::string, std::map< std::string, BlockInfo > > BlockInfoList;
	//struct that contain the stream id, type & coordinates
	typedef struct                                                     
	{	
		std::string streamId;
		int streamType;
		std::vector< std::pair< float, float > > value;	
	}streamXY;

	streamXY xy;
	streamXY tempXY;
     //coordinate list of a given stream
	std::vector< streamXY > streamCoordList;	                          

   // link name, model name
   std::map< std::string, std::map< std::string, std::string > > inLinkToModel;
   std::map< std::string, std::map< std::string, std::string > > outLinkToModel;
   //container to hold link points with stream name
   std::map< std::string, std::map< std::string, std::vector< std::pair< float, float > > > > linkPoints;
   std::map< std::string, std::map< std::string, int > > linkTypes;
   // model name with number
   //std::map< std::string, int > models;
   std::map<std::string, std::map< std::string, int > >models;
   // model name with icon location
   std::map< std::string, std::map< std::string, std::pair< float, float > > > iconLocations;
   // stream name to port ids from and to
   std::map< std::string, std::map< std::string, std::pair< int, int > > > streamPortIDS;

   void StripCharacters( std::string& data, const std::string& character );   

   std::string workingDir;
   int redundantID;

public:
	CASI::CASIDocument * aspendoc;

    //constructor
	BKPParser();  
    //deconstrutor                                                     
	~BKPParser();                                                     
	void OpenSimAndParse(const char *);   
    //opens the given file                                          
	void OpenSim(const char *);
     //close the file
	void closeFile();                                            
	void saveFile();  
	void saveAs(const char *);                
	void showAspen(bool);    
	void ReinitAspen();
    void ReinitBlock( const std::string& modname );
	void step();
    //returns total components
	int getNumComponents();
    //returns the filename of component
	std::string getBlockType( const std::string& blockName, const std::string& flowsheetName = NULL); 
    //returns the filename of component
	std::string getBlockID( const std::string& blockName, const std::string& flowsheetName = NULL);     
    //returns the x coordinates of component                                 
	float getXCoord(int);             
    //returns the y coordinates of component                                 
	float getYCoord(int);                              
    //returns X coord of one point of stream                
	float getStreamXCoord(int streamIndex, int coordIndex); 
    //returns y coord of one point of stream
	float getStreamYCoord(int streamIndex, int coordIndex); 
    //returns the stream's id
	std::string getStreamId(int);                         
    //returns the stream's type  
	int getStreamType(int);                
     //returns total number of the streams                 
	int getNumStream();           
    //returns the total number of points for a stream                         
	int getStreamSize(int index);                           
	bool isOpen();

	void SetWorkingDir( const std::string& dir );

	void CreateNetworkLinks( ves::open::xml::model::NetworkPtr subNetwork, const std::string& hierName );
   void CreateNetworkInformation( std::string& networkData );
   void ParseSubSystem(ves::open::xml::model::ModelPtr model, const std::string& networkName );
   std::string CreateNetwork( void );
   std::string GetInputModuleParamProperties( const std::string& modname, const std::string& paramName);
   std::string GetInputModuleParams( const std::string& modname);
   std::string GetOutputModuleParamProperties( const std::string& modname, const std::string& paramName);
   std::string GetOutputModuleParams( const std::string& modname);
   std::string GetStreamInputModuleParamProperties( const std::string& modname, const std::string& paramName);
   std::string GetStreamInputModuleParams( const std::string& modname);
   std::string GetStreamOutputModuleParamProperties( const std::string& modname, const std::string& paramName);
   std::string GetStreamOutputModuleParams( const std::string& modname);
};

#endif
