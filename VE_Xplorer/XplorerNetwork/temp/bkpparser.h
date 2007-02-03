//
//  BKPParser - parses Aspen bkp files to acquire information about Graphics
//  Terry E. Jordan Jr.  - SAIC
//  NETL/DOE
//

#ifndef BKPPARSER_H
#define BKPPARSER_H

#include <string>
#include <vector>

class BKPParser
{

private:
	void ParseFile(char *);                                            //parses the bkp file
	std::vector<float> xCoords;
	std::vector<float> yCoords;	

	typedef struct                                                     //struct with blocks type and id
	{
		std::string type;
		std::string icon;
		std::string id;
	}BlockInfo;

	std::vector<BlockInfo> BlockInfoList;

	typedef struct
	{
   		float x;
   		float y;
	}streamCoords;

	typedef struct                                                     //struct that contain the stream id, type & coordinates
	{	
		std::string streamId;
		int streamType;
		std::vector<streamCoords> value;	
	}streamXY;

	streamXY xy;
	streamXY tempXY;
	std::vector<streamXY> streamCoordList;	                           //coordinate list of a given stream
	std::vector<std::string> streamIds;                                //vector of stream ids

public:
	BKPParser();                                                       //constructor
	~BKPParser();                                                      //deconstrutor
	void openFile(char *);                                             //opens the given file
	int getNumComponents();                                            //returns total components
	std::string getBlockType(int);                                     //returns the filename of component
	std::string getBlockIcon(int);
	std::string getBlockID(int);                                       //returns the filename of component
	float getXCoord(int);                                              //returns the x coordinates of component
	float getYCoord(int);                                              //returns the y coordinates of component
	float getStreamXCoord(int streamIndex, int coordIndex); //returns X coord of one point of stream
	float getStreamYCoord(int streamIndex, int coordIndex); //returns y coord of one point of stream
	std::string getStreamId(int);                           //returns the stream's id
	int getStreamType(int);                                 //returns the stream's type
	int getNumStream();                                     //returns total number of the streams
	int getStreamSize(int index);                           //returns the total number of points for a stream
};

#endif
