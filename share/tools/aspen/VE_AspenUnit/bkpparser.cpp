
#include "stdafx.h"
#include <gdiplus.h>
#include "bkpparser.h"
#include <iostream>
#include <fstream>
#include <algorithm>
#include "VE_Open/XML/Model/Link.h"
#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/Model/Point.h"
#include "VE_Open/XML/Model/Port.h"
#include "VE_Open/XML/Command.h"
#include <fstream>
#include <iostream>
#include <cmath>

using namespace Gdiplus;

BKPParser::BKPParser()
{
	aspendoc = new CASI::CASIDocument();
	veNetwork = new VE_XML::VE_Model::Network();
}

BKPParser::~BKPParser()
{
}

void BKPParser::openFile(const char * file)
{
	std::string fileName(file);
	std::string bkpExt(".bkp");
	std::string apwExt(".apw");
	ParseFile((fileName + bkpExt).c_str());	
	CString filename = file;
	aspendoc->open((fileName + apwExt).c_str());
}


void BKPParser::closeFile()
{
	aspendoc->close();
	xCoords.clear();
	yCoords.clear();
	BlockInfoList.clear();
	xy.streamId.clear();
	xy.streamType = NULL;
	xy.value.clear();
	tempXY.streamId.clear();
	tempXY.streamType =  NULL;
	tempXY.value.clear();
	streamCoordList.clear();
	streamIds.clear();
	inLinkToModel.clear();
	outLinkToModel.clear();
	linkPoints.clear();
	models.clear();
	iconLocations.clear();
	streamPortIDS.clear();
}
void BKPParser::saveFile()
{
	aspendoc->save();
}
void BKPParser::saveAs(const char * filename)
{
	aspendoc->saveAs(filename);
}
void BKPParser::showAspen(bool show)
{
	aspendoc->showAspen(show);
}
void BKPParser::step()
{
	aspendoc->step();
}
int BKPParser::getNumComponents()
{
	return BlockInfoList.size(); //vectors are all same length
}

std::string BKPParser::getBlockType(std::string name)
{
	return BlockInfoList[name].type;
}

std::string BKPParser::getBlockID(std::string name)
{
	return BlockInfoList[name].id;
}

float BKPParser::getXCoord(int num)
{
	return xCoords[num];
}

float BKPParser::getYCoord(int num)
{
	return yCoords[num];
}

float BKPParser::getStreamXCoord(int streamIndex, int coordIndex)
{
	return streamCoordList[streamIndex].value[coordIndex].first;
}

float BKPParser::getStreamYCoord(int streamIndex, int coordIndex)
{
	return streamCoordList[streamIndex].value[coordIndex].second;
}

std::string BKPParser::getStreamId(int streamIndex)
{
	return streamCoordList[streamIndex].streamId;
}

int BKPParser::getStreamType(int streamIndex)
{
	return streamCoordList[streamIndex].streamType;
}

int BKPParser::getNumStream()
{
	return streamCoordList.size();
}

int BKPParser::getStreamSize(int index)
{
	return streamCoordList[index].value.size();
}

void BKPParser::ParseFile(const char * bkpFile)
{
	//Open file streams	
	std::ifstream inFile (bkpFile, std::ios::binary);
	std::ifstream lutFile ("F:/ASPENV21/LUT.txt");
	std::ofstream outFile("log.txt");
	std::string discard;
	
	//make sure it is a valid file
	if(!inFile)
	{
		std::cout<<"Error while opening File"<<std::endl;
		return;
	}
	else 
		std::cout<<"File was opened successfully"<<std::endl;
	
	//
	// Begin parsing
	//

	std::string temp;
	
	while(	temp.compare(0 , 7, "NumLibs", 0, 7))
	{
		getline(inFile, temp);
		if (inFile.eof())
			break;
	}

	std::stringstream numLibToken(temp);
	numLibToken >> discard;//Dump "NumLibs"
	numLibToken >> discard;//Dump "="
	int numLibs;
	numLibToken >> numLibs;
	int i=0;
	while(i<numLibs)
	{
		getline(inFile, temp);
		i++;
	}	
	
	getline(inFile, temp);
	
	std::stringstream numCatToken(temp);
	numCatToken >> discard;//Dump "NumLibs"
	numCatToken >> discard;//Dump "="
	int numCats;
	numCatToken >> numCats;
	
	i = 0;
	while(i<numCats*2+2)
	{
		getline(inFile, temp);
		i++;
	}	

	//get number of components/blocks
	getline(inFile, temp);
	int numComponents = atoi(temp.c_str());
	std::cout<<"Comp#: "<<numComponents<<std::endl;

	//
	// Block Info
	//

	std::cout<<"Acquired Graphics Information"<<std::endl;

	//Get block id and type
	int count = 0;
	std::string compVer, compID, compName, compLib, compLibName;
	BlockInfo tempBlockInfo;
	std::string hierarchy;
	bool hierFlag = false;
	std::ofstream hierfile("hierarchy.txt");
	while(count < numComponents)
	{
		getline(inFile, compVer);
		getline(inFile, compID);
		getline(inFile, compName);
		getline(inFile, compLib);
		getline(inFile, compLibName);
		std::cout<<compID<<std::endl;
		if(compID.find(".") == std::string::npos)
		{
			//add new block to vector
			std::stringstream tokenizer(compID);
			tokenizer >> tempBlockInfo.id;
			std::cout<<"add: "<<tempBlockInfo.id<<std::endl;
			//tokenizer.str(compName);
			tokenizer.str(compLibName);
			tokenizer >> tempBlockInfo.type;
			if(compLibName.find("HIERARCHY") == std::string::npos)
				tempBlockInfo.hierarchical = false;
			else
				tempBlockInfo.hierarchical = true;
			BlockInfoList[tempBlockInfo.id] = tempBlockInfo;
		}
		else
		{
			std::string tempHierarchyBlock;			
			size_t  pos = compID.find(".", 0);
			
			//get the heirarchy block
			std::string temp = compID.substr(0, pos);
			compID.erase(0, pos + 1);
			tempHierarchyBlock = temp;
			tempBlockInfo.id = compID;

			std::stringstream tokenizer(compLibName);
			tokenizer >> tempBlockInfo.type;
			
			tempBlockInfo.hierarchical = false;
			HierarchicalBlockInfoList[tempHierarchyBlock][tempBlockInfo.id] = tempBlockInfo;

			hierfile << HierarchicalBlockInfoList.size()<< " : "<< HierarchicalBlockInfoList[tempHierarchyBlock].size()<<std::endl;
			hierfile << "main: "<<tempHierarchyBlock<<" embedded: "<<tempBlockInfo.id<<std::endl;
		}
		count++;
	}
	hierfile.close();
	std::cout<<BlockInfoList.size()<<std::endl;
	std::cout<<"Aqcuired type/id list"<<std::endl;

   //The following block contains the network data
   std::streampos beforeNetwork;
   beforeNetwork = inFile.tellg();
   std::cout<<beforeNetwork<<std::endl;

	//find the graphics section
	while(temp.compare(0, 16, " GRAPHICS_BACKUP", 0, 16)!= 0 && !inFile.eof())
	{
		getline(inFile, temp);
	}
	std::cout<<"Found Graphics Section"<<std::endl;
   
	//Now we have passed the network data so record it
	std::streampos afterNetwork;
	afterNetwork = inFile.tellg();
	std::cout<<afterNetwork<<std::endl;
	//go back to the beginning of the network
	inFile.seekg( beforeNetwork );
	// allocate memory:
	char* buffer = new char [afterNetwork - beforeNetwork];
	// read data as a block:
	inFile.read( buffer, (afterNetwork - beforeNetwork) );
	std::ofstream tester4 ("tester4.txt");
	tester4<<buffer<<std::endl;
	tester4.close();
	std::string networkData( buffer );
	delete [] buffer;

	//build network information
	CreateNetworkInformation( networkData );
	
	//find first block
	while(temp.compare(0, 5, "BLOCK", 0, 5)!= 0 && !inFile.eof())
	{
		getline(inFile, temp);
	}
	std::cout<<"First Block Found"<<std::endl;
	
	//Read graphic blocks
	count =0;
	std::string id, version, icon, flag, section, at, labelAt, scaleMod, annotation;
	while(count < (int)BlockInfoList.size())
	{
		getline(inFile, id);
		std::cout<<id<<std::endl;
		getline(inFile, version);
		getline(inFile, icon);

		std::stringstream iconTokenizer(icon);
		iconTokenizer >> discard;
		
		getline(inFile, flag);
		getline(inFile, section);
		getline(inFile, at);
		getline(inFile, labelAt);
		getline(inFile, temp);
		//check for the optional line "Annotation"
		if(temp.compare(0, 10, "Annotation", 0, 10)== 0)
		{
			annotation = temp;
			getline(inFile, scaleMod);
		}
		else
		{
			scaleMod = temp;
		}

		float scale;
		int modifier;
		std::stringstream scaleTokenizer(scaleMod);
		scaleTokenizer >> discard; //discard "scale" string
		scaleTokenizer >> scale;   //grab scale
		scaleTokenizer >> discard; //discard "modifier" string
		scaleTokenizer >> modifier;//grab modifier

		getline(inFile, temp); //dump next block header Should BE CHANGED - when next entry is important
		
		//parse id to use for searching		
		std::stringstream idTokenizer(id);
		idTokenizer >> discard;
		std::string tempBlockId; 
		idTokenizer >> tempBlockId;

		BlockInfo tempBlockInfo_2;
		//sort the block id/type vector
		int entryIncr = 0;
		bool entryFound = false;
		std::string iconType;
		std::string tempIcon;
		iconTokenizer >> tempIcon;
		tempIcon = tempIcon.substr(1, tempIcon.size() - 2);
		BlockInfoList[tempBlockId].icon = tempIcon;
		BlockInfoList[tempBlockId].scale = scale;// * 0.5f;
		
		//find offset
		float left=0, right=0, bottom=0, top=0; //coords
		float widthOffset = 0;
		float heightOffset = 0;
		std::string tempEntry;
		std::string tempParser;
		outFile<<BlockInfoList[tempBlockId].type+" "+BlockInfoList[tempBlockId].icon<<std::endl;
		while(tempEntry.find(BlockInfoList[tempBlockId].type+" "+BlockInfoList[tempBlockId].icon, 0) == std::string::npos && !inFile.eof())
		{
			getline(lutFile, tempEntry);
		}
		lutFile.seekg(0);
		outFile<<tempEntry<<std::endl;
		//parse out percentage
		std::stringstream entryParser (tempEntry);
		for(int z = 0; z < 2; z++)
			entryParser >> tempParser;
		entryParser >> left;
		entryParser >> right;
		entryParser >> top;
		entryParser >> bottom;
		
		float iconWidth = right - left;
		float iconHeight = top - bottom;

		if(modifier == 0)
		{
			BlockInfoList[tempBlockId].rotation = 0.0f;
			BlockInfoList[tempBlockId].mirror = 0;
			widthOffset = abs(left/iconWidth);
			heightOffset = abs(top/iconHeight);
		}
		else if(modifier == 1)
		{
			BlockInfoList[tempBlockId].rotation = 0.0f;
			BlockInfoList[tempBlockId].mirror = 1;
			widthOffset = abs(right/iconWidth);
			heightOffset = abs(top/iconHeight);
		}
		else if(modifier == 2)
		{
			BlockInfoList[tempBlockId].rotation = 0.0f;
			BlockInfoList[tempBlockId].mirror = 2;
			widthOffset = abs(left/iconWidth);
			heightOffset = abs(bottom/iconHeight);
		}
		else if(modifier == 3)
		{
			BlockInfoList[tempBlockId].rotation = 90.0f;
			BlockInfoList[tempBlockId].mirror = 0;
			widthOffset = abs(top/iconWidth);
			heightOffset = abs(right/iconHeight);
		}
		else if(modifier == 4)
		{
			BlockInfoList[tempBlockId].rotation = 270.0f;
			BlockInfoList[tempBlockId].mirror = 0;
			widthOffset = abs(bottom/iconWidth);
			heightOffset = abs(left/iconHeight);
		}
		else if(modifier == 5)
		{
			BlockInfoList[tempBlockId].rotation = 0.0f;
			BlockInfoList[tempBlockId].mirror = 3;
			widthOffset = abs(right/iconWidth);
			heightOffset = abs(bottom/iconHeight);
		}
		else if(modifier == 6)
		{
			BlockInfoList[tempBlockId].rotation = 270.0f;
			BlockInfoList[tempBlockId].mirror = 2;
			widthOffset = abs(top/iconWidth);
			heightOffset = abs(left/iconHeight);
		}
		else if(modifier == 7)
		{
			BlockInfoList[tempBlockId].rotation = 270.0f;
			BlockInfoList[tempBlockId].mirror = 1;
			widthOffset = abs(bottom/iconWidth);
			heightOffset = abs(right/iconHeight);
		}

		//parse location to create coordinates for blocks		
		std::stringstream atTokenizer(at);
		atTokenizer >> discard;
		float xcoord, ycoord;
		atTokenizer >> xcoord;
		atTokenizer >> ycoord;
		xCoords.push_back(xcoord);
		yCoords.push_back(ycoord);
		
		//scaled up for icon spacing
		float scaledXCoords = xCoords.back() * 100;
		//invert Y axis - flowsheets are inverted
		float scaledYCoords = -yCoords.back() * 100;
		
		CString iconPath = ("2DIcons/"+BlockInfoList[tempBlockId].type+"/"+BlockInfoList[tempBlockId].type+"."+BlockInfoList[tempBlockId].icon+".jpg").c_str();
		LPWSTR lpszW = new WCHAR[255];
		LPTSTR lpStr = iconPath.GetBuffer( iconPath.GetLength() );
		int nLen = MultiByteToWideChar(CP_ACP, 0,lpStr, -1, NULL, NULL);
		MultiByteToWideChar(CP_ACP, 0, lpStr, -1, lpszW, nLen);
		
		GdiplusStartupInput gdiplusStartupInput;
		ULONG_PTR gdiplusToken;
		GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);
		Image * image = new Image(lpszW);
		float width = image->GetWidth();
		float height = image->GetHeight();
		GdiplusShutdown(gdiplusToken);

		//iconLocations[ tempBlockId ] = std::pair< float, float >( scaledXCoords+200, scaledYCoords+200 );
		//iconLocations[ tempBlockId ] = std::pair< float, float >( scaledXCoords + 500 - (0.25*width), scaledYCoords + 500 - (0.25*height) );
		iconLocations[ tempBlockId ] = std::pair< float, float >( scaledXCoords - (width*widthOffset*BlockInfoList[tempBlockId].scale), scaledYCoords - (height*heightOffset*BlockInfoList[tempBlockId].scale) );
		count++;
	}
	lutFile.close();
	std::cout<<"Finished Reading Block Info"<<std::endl;

	//locate minimum X - used for normalization
	float minX = 0;
	float minY = 0;
	std::map< std::string, std::pair< float, float > >::iterator iter;
	for (iter = iconLocations.begin(); iter != iconLocations.end(); iter++)
	{
		float currentX = iconLocations[ iter->first ].first;
		float currentY = iconLocations[ iter->first ].second;
		if(currentX < minX)
			minX = currentX;
		if(currentY < minY)
			minY = currentY;
	}

	//
	//Stream Info
	//

	std::cout<<"Begin Reading Streams"<<std::endl;
	//Gather stream information
	std::string streamId, streamVersion, streamFlag, streamType, coordinates, tempR, tempR2;
	bool newStream = true, routeOne = false;;
	std::pair< float, float > tempCoords;
	int routeCount = 0;
	std::ofstream tester2 ("tester2.txt"); 
	
	//contiously read all stream info to the legend or viewport entry
	while(temp.compare(0, 8, "VIEWPORT", 0, 8)!= 0 && temp.compare(0, 6, "LEGEND", 0, 6)!= 0 && !inFile.eof())
	{
		if(temp.compare(0, 6, "STREAM", 0, 6)== 0)//find "STREAM" entry
		{
			getline(inFile, streamId);
			tester2<<streamId<<": ";
			getline(inFile, streamVersion);
			getline(inFile, streamFlag);
			getline(inFile, streamType);
			//Look for Stream type - needed for version 13.2
			while(streamType.compare(0,4,"TYPE",0,4)!=0)
				getline(inFile, streamType);
			getline(inFile, temp);
			routeCount = 0;
			routeOne=false;
			newStream = true;
			
			//Look for Routes
			while(temp.compare(0, 6, "STREAM", 0, 6)!= 0 && temp.compare(0, 8, "VIEWPORT", 0, 8)!= 0 && temp.compare(0, 6, "LEGEND", 0, 6)!= 0 && routeCount <3 && streamId.find("#")==std::string::npos)
			{
				//look for ROUTE heading
				if(temp.compare(0, 5, "ROUTE", 0, 5) == 0)
				{
					getline(inFile, temp);
					routeCount ++;
				}
				//grab data
				else if(temp.compare(0,2,"r ",0,2)==0)
				{
					while(temp.compare(0,5,"ROUTE",0,5)!=0 && temp.compare(0,2,"$ ",0,2)!=0 &&temp.compare(0,2,"At",0,2)!=0 && temp.compare(0,5,"Label")!=0)
					{
						//seems you only need first 2 routes
						if(routeCount == 1 || routeCount ==2)
						//if(routeCount == 1)
						{
							std::stringstream streamTokenizer(temp);
							streamTokenizer >> tempR;
							streamTokenizer >> tempR2;
							streamTokenizer >> tempCoords.first;
							streamTokenizer >> tempCoords.second;

							if(routeCount == 1)
							{
								xy.value.push_back(tempCoords);
								routeOne = true;
							}
							if(routeCount == 2)
							{
								tempXY.value.push_back(tempCoords);
							}
						}
						else
							std::cout << "ERROR: "<<routeCount<<std::endl;
						getline(inFile, temp);
					}
				}
				else 
				{
					getline(inFile, temp);
				}
			}
			//put 2nd route entry data ahead of first in vector
			int tempCount=0;
			while(tempCount < (int)tempXY.value.size())
			{
				xy.value.insert(xy.value.begin(), tempXY.value[tempCount]);
				tempCount++;
			}
			
			std::stringstream idTokenizer(streamId);
			idTokenizer >> discard;
			idTokenizer >> xy.streamId;
			
			std::stringstream typeTokenizer(streamId);
			typeTokenizer >> discard;
			typeTokenizer >> xy.streamType;
			
			streamCoordList.push_back(xy); //add one streams values to vector      
			
			//Create map of stream names to points
			for ( size_t k = 0; k < xy.value.size(); ++k )
			{
				//scaled up for icon spacing
				//float scaledX = xy.value.at( k ).first * 40;
				float scaledX = xy.value.at( k ).first * 100;
				//invert Y axis - flowsheets are inverted
				//float scaledY = -xy.value.at( k ).second * 40;
				float scaledY = -xy.value.at( k ).second * 100;
				if(scaledX < minX)
					minX = scaledX;
				if(scaledY < minY)
					minY = scaledY;
				tester2<<" x: "<<scaledX<<" y: "<<scaledY;
				//linkPoints[xy.streamId].push_back( std::pair< float, float >( scaledX+200, scaledY+200 ) );
				//linkPoints[xy.streamId].push_back( std::pair< float, float >( scaledX + 1000, scaledY +1000 ) );
				linkPoints[xy.streamId].push_back( std::pair< float, float >( scaledX, scaledY ) );
			}
			// add converted points for wx
			xy.value.erase(xy.value.begin(),xy.value.end() );//empty temporary vector
			tempXY.value.erase(tempXY.value.begin(),tempXY.value.end() );//empty temporary vector
		}
		else 
			getline(inFile, temp);
		tester2<<std::endl;
	}

	std::cout<<"Finished Reading Streams"<<std::endl;
	
	//
	//NORMALIZE FOR WX
	//
	float normX = fabs(minX);
	float normY = fabs(minY);
	tester2<<"NormX: "<<normX<<" NormY: "<<normY<<std::endl;
	tester2.close();
	//blocks
	std::ofstream tester3 ("tester3.txt");
	for(iter = iconLocations.begin(); iter != iconLocations.end(); iter++)
	{
		iconLocations[ iter->first ].first = iconLocations[iter->first].first + normX;
		iconLocations[ iter->first ].second = iconLocations[iter->first].second + normY;
		//iconLocations[ iter->first ].first = iconLocations[iter->first].first;
		//iconLocations[ iter->first ].second = iconLocations[iter->first].second;
		tester3<<iter->first<<": x: "<<iconLocations[ iter->first ].first<<" y: "<<iconLocations[ iter->first ].second<<std::endl;
	}
	tester3.close();

	//streams
	std::ofstream tester ("tester.txt");
	std::map< std::string, std::vector< std::pair< float, float > > >::iterator iter2;
	for(iter2 = linkPoints.begin(); iter2 != linkPoints.end(); iter2++)
	{
		tester<<iter2->first<<":";
		for(int element = 0; element < (int)linkPoints[ iter2->first ].size(); element++)
		{
			linkPoints[ iter2->first ][element].first = linkPoints[ iter2->first ][element].first + normX;
			linkPoints[ iter2->first ][element].second = linkPoints[ iter2->first ][element].second + normY;
			//linkPoints[ iter2->first ][element].first = linkPoints[ iter2->first ][element].first;
			//linkPoints[ iter2->first ][element].second = linkPoints[ iter2->first ][element].second;
			tester<<" x: "<< linkPoints[ iter2->first ][element].first<<" y: "<<(float)linkPoints[ iter2->first ][element].second;
		}
		tester<<std::endl;
	}
	tester.close();


	//
	// Log
	//

	//create a log file
	std::cout<<"Writing log."<<std::endl;
	count = 0;
	int streamCount = 0;
	outFile << BlockInfoList.size()<<std::endl;
	while (count < (int)BlockInfoList.size())
	{
		outFile << xCoords[count];
		outFile << "\t";
		outFile << yCoords[count];
		outFile << "\n";
		count ++;
	}
	count=0;
	outFile << streamCoordList.size()<<std::endl;
	while(streamCount < (int)streamCoordList.size())
	{
		outFile<<streamCoordList[streamCount].value.size()<<std::endl;
		while(count < (int)streamCoordList[streamCount].value.size())
		{	
			outFile << streamCoordList[streamCount].value[count].first;
			outFile << "\t";
			outFile << streamCoordList[streamCount].value[count].second;
			outFile << "\n";
			count++;
		}
		streamCount++;
		count = 0;
	}
	
	std::cout<<"Parsing Completed!"<<std::endl;
	inFile.close();
	outFile.close();
	return;
}
////////////////////////////////////////////////////////////////////////
void BKPParser::CreateNetworkInformation( std::string networkData )
{
   // strip the new line characters
    StripCharacters( networkData, "\n" );
   // strip the <cr>
   StripCharacters( networkData, "\r" );
   /// Add code the reads the block network info
   /// Search for "? FLOWSHEET GLOBAL ?"
   /// then grab all sections that start with \ and end with \ tempe
   size_t networkBegin = networkData.find( std::string( "BLKID" ) );
   size_t networkEnd = networkData.find( std::string( "? PROPERTIES MAIN ?" ) );
   std::string network;
   network.append( networkData, networkBegin, (networkEnd - networkBegin) );
   
   size_t tag = 0;
   size_t index = 0;
   // create the maps and network connectivity
   std::string blockName;
   std::string discard;
   do
   {
      tag = network.find( std::string( "\\" ), index );
      std::string blockData;
      if ( tag != std::string::npos )
      {
         blockData.append( network, index, (tag - index) );
		 //std::cout<<blockData<<std::endl;
		 index = tag + 1;
		 if(blockData.find(std::string("COMMENTS")) == std::string::npos  &&
			blockData.find(std::string("FLOWSHEET")) == std::string::npos &&
			blockData.find(std::string("DEF-STREAM")) == std::string::npos&&
			blockData.find(std::string("CONNECT BLKID")) == std::string::npos)
		 {
			 std::stringstream networkToks(blockData);
			 std::stringstream tempTokens(networkToks.str());
			 //networkToks.str().clear();
			 int toksCounter = 0;
			 while (tempTokens >> discard)
				 toksCounter++;

			 std::vector< std::string >  vectorTokens;
			 for ( int i = 0; i < toksCounter; ++i )
			 {
				 std::string token;
				 networkToks >> token;
				 vectorTokens.push_back( token );
			 }
	         
			 // Now parse the vector of tokens...
			 for ( size_t i = 0; i < vectorTokens.size(); ++i )
			 {
				// This is the block names
				if ( (vectorTokens.at( i ) == std::string( "=" )) && (vectorTokens.at( i - 1 ) == std::string( "BLKID" )) )
				{
				   blockName = vectorTokens.at( ++i );
				   StripCharacters( blockName, "\"" );
				   models[ blockName ] = index;
				}
				// this are the input links/streams that connect to this particular block
				else if ( (vectorTokens.at( i ) == std::string ( "=" )) && (vectorTokens.at( i - 1 ) == std::string( "IN" )) )
				{
				   std::string tempStrem = vectorTokens.at( i+=2 );
				   StripCharacters( tempStrem, "\"" );
				   inLinkToModel[ tempStrem ] = blockName;
				   //std::cout << tempStrem << std::endl;
				   ++i;
	               
				   while ( vectorTokens.at( i+1 ) != std::string( ")" ) )
				   {
					  tempStrem = vectorTokens.at( ++i );
					  StripCharacters( tempStrem, "\"" );
					  inLinkToModel[ tempStrem ] = blockName;
					  ++i;
				   }
				}
				// this are the output links/streams that connect to this particular block
				else if ( (vectorTokens.at( i ) == std::string ( "=" )) && (vectorTokens.at( i - 1 ) == std::string( "OUT" )) )
				{
				   std::string tempStrem = vectorTokens.at( i+=2 );
				   StripCharacters( tempStrem, "\"" );
				   outLinkToModel[ tempStrem ] = blockName;
				   ++i;
	               
				   while ( vectorTokens.at( i+1 ) != std::string( ")" ) )
				   {
					  tempStrem = vectorTokens.at( ++i );
					  StripCharacters( tempStrem, "\"" );
					  outLinkToModel[ tempStrem ] = blockName;
					  ++i;
				   }             
				}
			 }
          }
		 //THIS IS USED TO GATHER HIERARCHY STREAM PORTS
		 else if(blockData.find(std::string("CONNECT BLKID")) != std::string::npos)
		 {
			 //parse the entry into tokens
			 std::stringstream networkToks(blockData);
			 std::stringstream tempTokens(networkToks.str());
			 //networkToks.str().clear();
			 int toksCounter = 0;
			 while (tempTokens >> discard)
				 toksCounter++;

			std::vector< std::string >  vectorTokens;
			for ( int i = 0; i < toksCounter; ++i )
			{
				std::string token;
				networkToks >> token;
				vectorTokens.push_back( token );
			}
			
			//iterate through the tokens
			for ( size_t i = 0; i < vectorTokens.size(); ++i )
			{
				// this are the input links/streams that connect to this particular block
				if ( (vectorTokens.at( i ) == std::string ( "=" )) && (vectorTokens.at( i - 1 ) == std::string( "IN" )) )
				{
				   std::string tempStrem = vectorTokens.at( i+=2 );
				   //if it has a "." it is the internal connection
				   if(tempStrem.find(".") == std::string::npos)
				   {
					   StripCharacters( tempStrem, "\"" );
					   inLinkToModel[ tempStrem ] = blockName;
					   ++i;
					   
					   while ( vectorTokens.at( i+1 ) != std::string( ")" ) )
					   {
						  tempStrem = vectorTokens.at( ++i );
						  StripCharacters( tempStrem, "\"" );
						  inLinkToModel[ tempStrem ] = blockName;
						  ++i;
					   }
				   }
				}
				// this are the output links/streams that connect to this particular block
				else if ( (vectorTokens.at( i ) == std::string ( "=" )) && (vectorTokens.at( i - 1 ) == std::string( "OUT" )) )
				{
				   std::string tempStrem = vectorTokens.at( i+=2 );
				   //if it has a "." it is the internal connection
				   if(tempStrem.find(".") == std::string::npos)
				   {
					   StripCharacters( tempStrem, "\"" );
					   outLinkToModel[ tempStrem ] = blockName;
					   ++i;
					   
					   while ( vectorTokens.at( i+1 ) != std::string( ")" ) )
					   {
						  tempStrem = vectorTokens.at( ++i );
						  StripCharacters( tempStrem, "\"" );
						  outLinkToModel[ tempStrem ] = blockName;
						  ++i;
					   }  
				   }
				}
			}
		 }
	   }
   }
   while( tag != std::string::npos );
}
///////////////////////////////////////////////////////////
void BKPParser::CreateNetworkLinks( void )
{
   // remove duplicate points
   std::map< std::string, std::vector< std::pair< float, float > > >::iterator pointsIter;
   for ( pointsIter = linkPoints.begin(); pointsIter != linkPoints.end(); ++pointsIter )
   {
      //std::vector< std::pair< unsigned int, unsigned int > > tempPoints;
      std::vector< std::pair< float, float > > tempPoints;
      tempPoints = pointsIter->second;
      std::vector< std::pair< float, float > >::iterator pairIter;
      for ( pairIter = tempPoints.begin(); pairIter != tempPoints.end(); )
      {
         // need to remove duplicate points
         std::vector< std::pair< float, float > >::iterator tempPairIter;
         tempPairIter = std::find( pairIter+1, tempPoints.end(), *pairIter );
         if ( tempPairIter != tempPoints.end() )
         {
            tempPoints.erase( tempPairIter );
         }
         else
            ++pairIter;
      }
      pointsIter->second = tempPoints;
   }

   std::map< std::string, std::string >::iterator iter;
   // create links for the network
   int counter = 0;
   for ( iter = inLinkToModel.begin(); iter != inLinkToModel.end(); ++iter )
   {
      std::map< std::string, std::string >::iterator fromModel;
      fromModel = outLinkToModel.find( iter->first );
      if ( fromModel != outLinkToModel.end() )
      {
         //define link
         // these are unique remember...
         std::string toPortName = iter->first;
         // these are unique remember...
         std::string fromPortName = fromModel->first;
         std::string toModelName = iter->second;
         std::string fromModelName = fromModel->second;

         int toPortId = counter++;
         int fromPortId = counter++;
         int toModelId = models[ toModelName ];
         int fromModelId = models[ fromModelName ];
         streamPortIDS[ iter->first ] = std::pair< int, int >( toPortId, fromPortId );
         
		 //Now we create a link
         VE_XML::VE_Model::Link* xmlLink = veNetwork->GetLink( -1 );
         xmlLink->GetFromModule()->SetData( fromModelName, static_cast< long int >( fromModelId ) );
         xmlLink->GetToModule()->SetData( toModelName, static_cast< long int >( toModelId ) );
         //xmlLink->SetLinkName("TEST");
		 xmlLink->SetLinkName(iter->first);
         *(xmlLink->GetFromPort()) = static_cast< long int >( fromPortId );
         *(xmlLink->GetToPort()) = static_cast< long int >( toPortId );
         //Try to store link cons,
         //link cons are (x,y) wxpoint
         //here I store x in one vector and y in the other
         for ( size_t j = linkPoints[ fromPortName ].size(); j > 0 ; --j )
         {
            // I am not sure why we need to reverse the points but we do
            xmlLink->GetLinkPoint( linkPoints[ fromPortName ].size() - j )->SetPoint( linkPoints[ fromPortName ].at( j - 1 ) );
         }
      }
   }
}
///////////////////////////////////////////////////////////////////////
std::string BKPParser::CreateNetwork( void )
{
   // then create the appropriate models
   // then put them all together and for a network string
   // Here we wshould loop over all of the following
   std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
   
   nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( veNetwork, "veNetwork" ) );
   
   // create default state info section
   veNetwork->GetDataValuePair( -1 )->SetData( "m_xUserScale", 1.0 );
   veNetwork->GetDataValuePair( -1 )->SetData( "m_yUserScale", 1.0 );
   veNetwork->GetDataValuePair( -1 )->SetData( "nPixX", static_cast< long int >( 20 ) );
   veNetwork->GetDataValuePair( -1 )->SetData( "nPixY", static_cast< long int >( 20 ) );
   veNetwork->GetDataValuePair( -1 )->SetData( "nUnitX", static_cast< long int >( 200 ) );
   veNetwork->GetDataValuePair( -1 )->SetData( "nUnitY", static_cast< long int >( 200 ) );

   // Create links section
   CreateNetworkLinks();

   //  Models
   std::map< std::string, int >::iterator iter;
   int blockCount = 0;
   for ( iter=models.begin(); iter!=models.end(); ++iter )
   {
	   VE_XML::VE_Model::Model* tempModel = new VE_XML::VE_Model::Model();
      tempModel->SetModelID( iter->second );
      tempModel->SetModelName( iter->first );
      tempModel->SetVendorName( "ASPENUNIT" );
	  tempModel->SetIconFilename(BlockInfoList[iter->first].type+"/"+BlockInfoList[iter->first].type+"."+BlockInfoList[iter->first].icon);
	  tempModel->SetIconRotation(BlockInfoList[iter->first].rotation);
	  tempModel->SetIconScale(BlockInfoList[iter->first].scale);
	  tempModel->SetIconMirror(BlockInfoList[iter->first].mirror);
      blockCount++;
	  tempModel->GetIconLocation()->SetPoint( std::pair< double, double >( iconLocations[ iter->first ].first, iconLocations[ iter->first ].second ) );
      std::map< std::string, std::string >::iterator iterStreams;

	//search through input/output positions to find max X and max Y
	double maxX = iconLocations[ iter->first ].first;// + 40.0;
	double maxY = iconLocations[ iter->first ].second;// + 40.0;
	double minX = iconLocations[ iter->first ].first;
	double minY = iconLocations[ iter->first ].second;

	//40 is ICON size
	//double dx = 40.0/(maxX-minX);
	//double dy = 40.0/(maxY-minY);
	double dx = 1;
	double dy = 1;
	std::cout<<"maxX: "<<maxX<<" minX: "<<minX<<" maxY: "<<maxY<<" minY: "<<minY<<std::endl;

      // input ports
      for ( iterStreams = inLinkToModel.begin(); iterStreams != inLinkToModel.end(); ++iterStreams )
      {
         if ( iterStreams->second == iter->first )
         {
			 VE_XML::VE_Model::Port* tempPort = tempModel->GetPort(-1);
            // inputs are to ports
            tempPort->SetPortNumber( streamPortIDS[ iterStreams->first ].first );
            tempPort->SetModelName( iterStreams->first );
            tempPort->SetDataFlowDirection( std::string( "input" ) );
			//tempPort->GetPortLocation()->SetPoint( std::pair< double, double >( (linkPoints[tempPort->GetModelName()][0].first-minX)*dx, (linkPoints[tempPort->GetModelName()][0].second-minY)*dy ) );
			tempPort->GetPortLocation()->SetPoint( std::pair< double, double >( (linkPoints[tempPort->GetModelName()][0].first - minX ), (linkPoints[tempPort->GetModelName()][0].second - minY ) ) );
		 }
      }
      // output ports
      for ( iterStreams = outLinkToModel.begin(); iterStreams != outLinkToModel.end(); ++iterStreams )
      {
         if ( iterStreams->second == iter->first )
         {
			 VE_XML::VE_Model::Port* tempPort = tempModel->GetPort(-1);
            // outputs are from ports
            tempPort->SetPortNumber( streamPortIDS[ iterStreams->first ].second );
            tempPort->SetModelName( iterStreams->first );
            tempPort->SetDataFlowDirection( std::string( "output" ) );
			//tempPort->GetPortLocation()->SetPoint( std::pair< double, double >( (linkPoints[tempPort->GetModelName()][linkPoints[tempPort->GetModelName()].size()-1].first-minX)*dx, (linkPoints[tempPort->GetModelName()][linkPoints[tempPort->GetModelName()].size()-1].second-minY)*dy ) );
			tempPort->GetPortLocation()->SetPoint( std::pair< double, double >( (linkPoints[tempPort->GetModelName()][linkPoints[tempPort->GetModelName()].size()-1].first - minX ), (linkPoints[tempPort->GetModelName()][linkPoints[tempPort->GetModelName()].size()-1].second - minY ) ) );
         }
      }
      // temp data container for all the xmlobjects
      nodes.push_back( 
                  std::pair< VE_XML::XMLObject*, std::string >( tempModel, "veModel" ) 
                     );
   }
   std::string fileName( "returnString" );
   VE_XML::XMLReaderWriter netowrkWriter;
   netowrkWriter.UseStandaloneDOMDocumentManager();
   netowrkWriter.WriteXMLDocument( nodes, fileName, "Network" );
   return fileName;
}
////////////////////////////////////////////////////////////////////////////////
void BKPParser::StripCharacters( std::string& data, std::string character )
{
   for ( size_t index = 0; index < data.length(); )
   {
      index = data.find( character, index );
      if ( index != std::string::npos )
         data.erase( index, 1 );
   }
}
////////////////////////////////////////////////////////////////////////////////
//BLOCKS
std::string BKPParser::GetInputModuleParams(std::string modname)
{
	CASI::CASIObj cur_block= aspendoc->getBlockByName(CString(modname.c_str()));
	int i;
	
	VE_XML::Command params;
	std::vector<std::string> paramList;
	//input variables;
	params.SetCommandName((modname+"InputParams").c_str());
	
	for (i = 0; i < (int)cur_block.getNumInputVar(); i++)
	{	
        paramList.push_back((char*)LPCTSTR(cur_block.getInputVarName(i)));
    }

	VE_XML::DataValuePairWeakPtr inpParams = new VE_XML::DataValuePair();
	inpParams->SetData("params",paramList);
    params.AddDataValuePair( inpParams );
    
	std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
	nodes.push_back( std::pair< VE_XML::XMLObject*, 
        std::string >( &params, "vecommand" ) );

	VE_XML::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}
////////////////////////////////////////////////////////////////////////////////
std::string BKPParser::GetInputModuleParamProperties(std::string modname, std::string paramName)
{
	CASI::CASIObj cur_block= aspendoc->getBlockByName(CString(modname.c_str()));
	std::cout<<modname<<std::endl;
	std::cout<<paramName<<std::endl;
	unsigned int j;
	
	VE_XML::Command properties;
	const int propSize=23;
	properties.SetCommandName((modname+paramName).c_str());
	std::cout<<(modname+paramName).c_str()<<std::endl;

	VE_XML::DataValuePairWeakPtr Props[propSize];
	for (j=0; j<propSize; j++)
	{
		Props[j] = new VE_XML::DataValuePair();
		Props[j]->SetDataType("STRING");
		properties.AddDataValuePair( Props[j] );
	}
	//CASI::Variable tempvar = cur_block.getOutputVarByName(paramName.c_str());
	CASI::Variable tempvar = cur_block.getInputVarByName(CString(paramName.c_str()));
	j=0;
	Props[j]->SetDataName("Name");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getName()));
	
	Props[j]->SetDataName("NodePath");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getNodePath()));
	
	Props[j]->SetDataName("AliasName");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getAliasName()));
	
	Props[j]->SetDataName("Basis");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getBasis()));
	
	Props[j]->SetDataName("CompletionStatus");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getCompletionStatus()));
	
	Props[j]->SetDataName("DefaultValue");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getDefaultValue()));
	
	Props[j]->SetDataName("Gender");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getGender()));
	
	Props[j]->SetDataName("InorOut");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getInorOut()));
	
	Props[j]->SetDataName("Multiport");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getMultiport()));
	
	Props[j]->SetDataType("UNSIGNED INT");
	Props[j]->SetDataName("NumChild");
	Props[j++]->SetDataValue((unsigned int)(tempvar.getNumChild()));
	
	Props[j]->SetDataName("OptionList");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getOptionList()));
	
	Props[j]->SetDataName("Options");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getOptions()));
	
	Props[j]->SetDataName("PhysicalQuantity");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getPhysicalQuantity()));
	
	Props[j]->SetDataName("PortType");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getPortType()));
	
	Props[j]->SetDataName("Prompt");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getPrompt()));
	
	Props[j]->SetDataName("RecordType");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getRecordType()));
	
	Props[j]->SetDataName("UnitOfMeasure");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getUnitOfMeasure()));
	
	Props[j]->SetDataName("Value");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getValue()));
	
	Props[j]->SetDataName("hasChild");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.hasChild()));
	
	Props[j]->SetDataName("isEnterable");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.isEnterable()));
	
	Props[j]->SetDataName("isOutput");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.isOutput()));
	
	Props[j]->SetDataName("upLimit");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.upLimit()));
	
	Props[j]->SetDataName("lowerLimit");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.lowerLimit()));

	std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;

	nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( &properties, "vecommand" ) );

	VE_XML::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}

std::string BKPParser::GetOutputModuleParams(std::string modname)
{
	CASI::CASIObj cur_block= aspendoc->getBlockByName(CString(modname.c_str()));
	int i;
	
	VE_XML::Command params;
	std::vector<std::string> paramList;
	//input variables;
	params.SetCommandName((modname+"OutputParams").c_str());
	
	for (i = 0; i < (int)cur_block.getNumOutputVar(); i++)
		paramList.push_back((char*)LPCTSTR(cur_block.getOutputVarName(i)));

	VE_XML::DataValuePairWeakPtr inpParams = new VE_XML::DataValuePair();
	inpParams->SetData("params",paramList);
    params.AddDataValuePair( inpParams );

	std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
	nodes.push_back( 
                  std::pair< VE_XML::XMLObject*, std::string >( &params, "vecommand" ) 
                     );

	VE_XML::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}

std::string BKPParser::GetOutputModuleParamProperties(std::string modname, std::string paramName)
{
	CASI::CASIObj cur_block= aspendoc->getBlockByName(CString(modname.c_str()));
	std::cout<<modname<<std::endl;
	std::cout<<paramName<<std::endl;
	unsigned int j;
	
	VE_XML::Command properties;
	const int propSize=23;
	properties.SetCommandName((modname+paramName).c_str());
	std::cout<<(modname+paramName).c_str()<<std::endl;

	VE_XML::DataValuePairWeakPtr Props[propSize];
	for (j=0; j<propSize; j++)
	{
		Props[j] = new VE_XML::DataValuePair();
		Props[j]->SetDataType("STRING");
		properties.AddDataValuePair( Props[j] );
	}
	CASI::Variable tempvar = cur_block.getOutputVarByName(CString(paramName.c_str()));
	j=0;
	
	Props[j]->SetDataName("Name");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getName()));
	
	Props[j]->SetDataName("NodePath");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getNodePath()));
	
	Props[j]->SetDataName("AliasName");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getAliasName()));
	
	Props[j]->SetDataName("Basis");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getBasis()));
	
	Props[j]->SetDataName("CompletionStatus");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getCompletionStatus()));
	
	Props[j]->SetDataName("DefaultValue");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getDefaultValue()));
	
	Props[j]->SetDataName("Gender");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getGender()));
	
	Props[j]->SetDataName("InorOut");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getInorOut()));
	
	Props[j]->SetDataName("Multiport");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getMultiport()));
	
	Props[j]->SetDataType("UNSIGNED INT");
	Props[j]->SetDataName("NumChild");
	Props[j++]->SetDataValue((unsigned int)(tempvar.getNumChild()));
	
	Props[j]->SetDataName("OptionList");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getOptionList()));
	
	Props[j]->SetDataName("Options");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getOptions()));
	
	Props[j]->SetDataName("PhysicalQuantity");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getPhysicalQuantity()));
	
	Props[j]->SetDataName("PortType");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getPortType()));
	
	Props[j]->SetDataName("Prompt");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getPrompt()));
	
	Props[j]->SetDataName("RecordType");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getRecordType()));
	
	Props[j]->SetDataName("UnitOfMeasure");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getUnitOfMeasure()));
	
	Props[j]->SetDataName("Value");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getValue()));
	
	Props[j]->SetDataName("hasChild");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.hasChild()));
	
	Props[j]->SetDataName("isEnterable");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.isEnterable()));
	
	Props[j]->SetDataName("isOutput");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.isOutput()));
	
	Props[j]->SetDataName("upLimit");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.upLimit()));
	
	Props[j]->SetDataName("lowerLimit");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.lowerLimit()));

	std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;

	nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( &properties, "vecommand" ) );

	VE_XML::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}

//Streams
std::string BKPParser::GetStreamInputModuleParams(std::string modname)
{
	//CASI::CASIObj cur_block= aspendoc->getStreamByName(CString(modname.c_str()));
	CASI::CASIObj cur_stream= aspendoc->getStreamByName(CString(modname.c_str()));
	int i;
	
	VE_XML::Command params;
	std::vector<std::string> paramList;
	//input variables;
	params.SetCommandName((modname+"InputParams").c_str());
	int test = (int)cur_stream.getNumInputVar();
	for (i = 0; i < (int)cur_stream.getNumInputVar(); i++)
		//paramList.push_back((char*)LPCTSTR(cur_stream.getStreamCompName(i)));
		paramList.push_back((char*)LPCTSTR(cur_stream.getInputVarName(i)));

	VE_XML::DataValuePairWeakPtr inpParams = new VE_XML::DataValuePair();
	inpParams->SetData("params",paramList);
    params.AddDataValuePair( inpParams );

	std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
	nodes.push_back( 
                  std::pair< VE_XML::XMLObject*, std::string >( &params, "vecommand" ) 
                     );

	VE_XML::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}

std::string BKPParser::GetStreamInputModuleParamProperties(std::string modname, std::string paramName)
{
	CASI::CASIObj cur_stream= aspendoc->getStreamByName(CString(modname.c_str()));
	std::cout<<modname<<std::endl;
	std::cout<<paramName<<std::endl;
	unsigned int j;
	
	VE_XML::Command properties;
	const int propSize=23;
	properties.SetCommandName((modname+paramName).c_str());
	std::cout<<(modname+paramName).c_str()<<std::endl;

	VE_XML::DataValuePairWeakPtr Props[propSize];
	for (j=0; j<propSize; j++)
	{
		Props[j] = new VE_XML::DataValuePair();
		Props[j]->SetDataType("STRING");
		properties.AddDataValuePair( Props[j] );
	}
	CASI::Variable tempvar = cur_stream.getInputVarByName(paramName.c_str());
	//CASI::Variable tempvar = cur_stream.GetStreamComponentVarByName(CString(paramName.c_str()));
	j=0;
	Props[j]->SetDataName("Name");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getName()));
	
	Props[j]->SetDataName("NodePath");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getNodePath()));
	
	Props[j]->SetDataName("AliasName");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getAliasName()));
	
	Props[j]->SetDataName("Basis");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getBasis()));
	
	Props[j]->SetDataName("CompletionStatus");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getCompletionStatus()));
	
	Props[j]->SetDataName("DefaultValue");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getDefaultValue()));
	
	Props[j]->SetDataName("Gender");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getGender()));
	
	Props[j]->SetDataName("InorOut");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getInorOut()));
	
	Props[j]->SetDataName("Multiport");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getMultiport()));
	
	Props[j]->SetDataType("UNSIGNED INT");
	Props[j]->SetDataName("NumChild");
	Props[j++]->SetDataValue((unsigned int)(tempvar.getNumChild()));
	
	Props[j]->SetDataName("OptionList");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getOptionList()));
	
	Props[j]->SetDataName("Options");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getOptions()));
	
	Props[j]->SetDataName("PhysicalQuantity");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getPhysicalQuantity()));
	
	Props[j]->SetDataName("PortType");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getPortType()));
	
	Props[j]->SetDataName("Prompt");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getPrompt()));
	
	Props[j]->SetDataName("RecordType");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getRecordType()));
	
	Props[j]->SetDataName("UnitOfMeasure");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getUnitOfMeasure()));
	
	Props[j]->SetDataName("Value");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getValue()));
	
	Props[j]->SetDataName("hasChild");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.hasChild()));
	
	Props[j]->SetDataName("isEnterable");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.isEnterable()));
	
	Props[j]->SetDataName("isOutput");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.isOutput()));
	
	Props[j]->SetDataName("upLimit");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.upLimit()));
	
	Props[j]->SetDataName("lowerLimit");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.lowerLimit()));

	std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;

	nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( &properties, "vecommand" ) );

	VE_XML::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}

std::string BKPParser::GetStreamOutputModuleParams(std::string modname)
{
	CASI::CASIObj cur_stream= aspendoc->getStreamByName(CString(modname.c_str()));
	int i;
	
	VE_XML::Command params;
	std::vector<std::string> paramList;
	//input variables;
	params.SetCommandName((modname+"OutputParams").c_str());
	
	for (i = 0; i < (int)cur_stream.getNumOutputVar(); i++)
		paramList.push_back((char*)LPCTSTR(cur_stream.getOutputVarName(i)));

	VE_XML::DataValuePairWeakPtr inpParams = new VE_XML::DataValuePair();
	inpParams->SetData("params",paramList);
    params.AddDataValuePair( inpParams );

	std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
	nodes.push_back( 
                  std::pair< VE_XML::XMLObject*, std::string >( &params, "vecommand" ) 
                     );

	VE_XML::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}

std::string BKPParser::GetStreamOutputModuleParamProperties(std::string modname, std::string paramName)
{
	CASI::CASIObj cur_stream= aspendoc->getStreamByName(CString(modname.c_str()));
	std::cout<<modname<<std::endl;
	std::cout<<paramName<<std::endl;
	unsigned int j;
	
	VE_XML::Command properties;
	const int propSize=23;
	properties.SetCommandName((modname+paramName).c_str());
	std::cout<<(modname+paramName).c_str()<<std::endl;

	VE_XML::DataValuePairWeakPtr Props[propSize];
	for (j=0; j<propSize; j++)
	{
		Props[j] = new VE_XML::DataValuePair();
		Props[j]->SetDataType("STRING");
		properties.AddDataValuePair( Props[j] );
	}
	CASI::Variable tempvar = cur_stream.getOutputVarByName(CString(paramName.c_str()));
	j=0;
	
	Props[j]->SetDataName("Name");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getName()));
	
	Props[j]->SetDataName("NodePath");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getNodePath()));
	
	Props[j]->SetDataName("AliasName");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getAliasName()));
	
	Props[j]->SetDataName("Basis");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getBasis()));
	
	Props[j]->SetDataName("CompletionStatus");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getCompletionStatus()));
	
	Props[j]->SetDataName("DefaultValue");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getDefaultValue()));
	
	Props[j]->SetDataName("Gender");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getGender()));
	
	Props[j]->SetDataName("InorOut");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getInorOut()));
	
	Props[j]->SetDataName("Multiport");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getMultiport()));
	
	Props[j]->SetDataType("UNSIGNED INT");
	Props[j]->SetDataName("NumChild");
	Props[j++]->SetDataValue((unsigned int)(tempvar.getNumChild()));
	
	Props[j]->SetDataName("OptionList");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getOptionList()));
	
	Props[j]->SetDataName("Options");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getOptions()));
	
	Props[j]->SetDataName("PhysicalQuantity");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getPhysicalQuantity()));
	
	Props[j]->SetDataName("PortType");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getPortType()));
	
	Props[j]->SetDataName("Prompt");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getPrompt()));
	
	Props[j]->SetDataName("RecordType");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getRecordType()));
	
	Props[j]->SetDataName("UnitOfMeasure");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getUnitOfMeasure()));
	
	Props[j]->SetDataName("Value");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.getValue()));
	
	Props[j]->SetDataName("hasChild");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.hasChild()));
	
	Props[j]->SetDataName("isEnterable");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.isEnterable()));
	
	Props[j]->SetDataName("isOutput");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.isOutput()));
	
	Props[j]->SetDataName("upLimit");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.upLimit()));
	
	Props[j]->SetDataName("lowerLimit");
	Props[j++]->SetDataString((char*)LPCTSTR(tempvar.lowerLimit()));

	std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;

	nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( &properties, "vecommand" ) );

	VE_XML::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}