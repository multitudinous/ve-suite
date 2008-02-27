#include "stdafx.h"
#include <gdiplus.h>
#include "bkpparser.h"
#include <iostream>
#include <fstream>
#include <algorithm>
#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/Command.h>
#include <fstream>
#include <iostream>
#include <cmath>

#include "AspenPlusLUT.h"
#include "AspenIconData.h"

using namespace Gdiplus;

BKPParser::BKPParser()
{
	aspendoc = new CASI::CASIDocument();
	//veNetwork = new VE_XML::VE_Model::Network();
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
	linkTypes.clear();
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

std::string BKPParser::getBlockType(std::string blockName, std::string flowsheetName)
{
	if(flowsheetName == "NULL")
		//return BlockInfoList["Top_Sheet"][blockName].type;
		return BlockInfoList["0"][blockName].type;
	else
		return BlockInfoList[flowsheetName][blockName].type;
}

std::string BKPParser::getBlockID(std::string blockName, std::string flowsheetName)
{
	if(flowsheetName == "NULL")
		//return BlockInfoList["Top_Sheet"][blockName].id;
		return BlockInfoList["0"][blockName].id;
	else
		return BlockInfoList[flowsheetName][blockName].id;
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
	std::ifstream inFile(bkpFile, std::ios::binary);

    std::map< std::pair< std::string, std::string >, std::vector< double > > lutMap;
    std::map< std::pair< std::string, std::string >, std::vector< double > >::iterator lutMapIter;
    std::vector< double > lutVector;
    lutVector.resize( 6 );
    
    lutMap = GetAspenPlusLUT();
	//std::ifstream lutFile ("LUT.txt");
	std::ofstream outFile("log.txt");
	std::string discard;
	
	std::map< std::string, std::pair< unsigned int, unsigned int > > imageData;
	imageData = GetAspenIconData();

	//make sure it is a valid file
	if(!inFile.is_open())
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
		/*if(compID.find(".") == std::string::npos)
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
		}*/
		//else
		//{
			//std::string tempHierarchyBlock;
			//size_t  pos = compID.find(".", 0);

			//get the heirarchy block
			//std::string temp = compID.substr(0, pos);
			//compID.erase(0, pos + 1);
			//tempHierarchyBlock = temp;
			//tempBlockInfo.id = compID;

			//remove newline
			std::stringstream tokenizer(compLibName);
			tokenizer >> tempBlockInfo.type;
			
			if(compLibName.find("HIERARCHY") == std::string::npos)
				tempBlockInfo.hierarchical = false;
			else
				tempBlockInfo.hierarchical = true;

			if(compID.find(".") == std::string::npos)
			{
				std::stringstream tokenizer(compID);
				tokenizer >> tempBlockInfo.id;  //remove newline
				//tempBlockInfo.id = compID;
				//HierarchicalBlockInfoList[tempHierarchyBlock][tempBlockInfo.id] = tempBlockInfo;
				//BlockInfoList["Top_Sheet"][tempBlockInfo.id] = tempBlockInfo;
				BlockInfoList["0"][tempBlockInfo.id] = tempBlockInfo;
			}
			else
			{
				//parse out the name of the hierarchy block
				std::stringstream tokenizer(compID);
				tokenizer >> compID;  //remove newline
				size_t  pos = compID.find_last_of(".");
				std::string temp = compID.substr(0, pos);
				tempBlockInfo.id = compID.substr(pos+1, compID.size());
				//HierarchicalBlockInfoList[tempHierarchyBlock][tempBlockInfo.id] = tempBlockInfo;
				BlockInfoList[temp][tempBlockInfo.id] = tempBlockInfo;
			}

			//hierfile << HierarchicalBlockInfoList.size()<< " : "<< HierarchicalBlockInfoList[tempHierarchyBlock].size()<<std::endl;
			//hierfile << "main: "<<tempHierarchyBlock<<" embedded: "<<tempBlockInfo.id<<std::endl;
		//}
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
	//tester4<<buffer<<std::endl;
	//tester4.close();
	std::string networkData( buffer );
	delete [] buffer;

	//build network information
	CreateNetworkInformation( networkData );
	////////////////////////Loop FOR Hierarchy///////////////////////////
	//std::map< std::string, std::map< std::string, BlockInfo > >::reverse_iterator sheetIter;
	std::map< std::string, std::map< std::string, BlockInfo > >::iterator sheetIter;
    //for (sheetIter = BlockInfoList.rbegin(); sheetIter != BlockInfoList.rend(); ++sheetIter)
    for (sheetIter = BlockInfoList.begin(); sheetIter != BlockInfoList.end(); ++sheetIter)
    {
        //find first block
        while(temp.compare(0, 5, "BLOCK", 0, 5)!= 0 && !inFile.eof())
        {
            getline(inFile, temp);
        }
        std::cout<<"First Block Found"<<std::endl;
        
        //Read graphic blocks
        count =0;
        std::string id, version, icon, flag, section, at, labelAt, scaleMod, annotation;
        while(count < (int)BlockInfoList[sheetIter->first].size())
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
            BlockInfoList[sheetIter->first][tempBlockId].icon = tempIcon;
            BlockInfoList[sheetIter->first][tempBlockId].scale = scale;// * 0.5f;
            
            //find offset
            float left=0, right=0, bottom=0, top=0; //coords
            float widthOffset = 0;
            float heightOffset = 0;
            //std::string tempEntry;
            //std::string tempParser;
            outFile<<BlockInfoList[sheetIter->first][tempBlockId].type+" "+BlockInfoList[sheetIter->first][tempBlockId].icon<<std::endl;
            //std::string remove = BlockInfoList[sheetIter->first][tempBlockId].type+" "+BlockInfoList[sheetIter->first][tempBlockId].icon;
            std::pair< std::string, std::string > blockKey( BlockInfoList[sheetIter->first][tempBlockId].type, BlockInfoList[sheetIter->first][tempBlockId].icon );
            lutMapIter = lutMap.find( blockKey );
            if( lutMapIter != lutMap.end() )
            {
                lutVector = lutMapIter->second;
                left = lutVector[ 0 ];
                right = lutVector[ 1 ];
                top = lutVector[ 2 ];
                bottom = lutVector[ 3 ];
            }
            else
            {
                left = 0;
                right = 0;
                top = 0;
                bottom = 0;
            }
            /*while(tempEntry.find(BlockInfoList[sheetIter->first][tempBlockId].type+" "+BlockInfoList[sheetIter->first][tempBlockId].icon, 0) == std::string::npos && !lutFile.eof())
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
            */
            
            float iconWidth = right - left;
            float iconHeight = top - bottom;

            if(modifier == 0)
            {
                BlockInfoList[sheetIter->first][tempBlockId].rotation = 0.0f;
                BlockInfoList[sheetIter->first][tempBlockId].mirror = 0;
                widthOffset = abs(left/iconWidth);
                heightOffset = abs(top/iconHeight);
            }
            else if(modifier == 1)
            {
                BlockInfoList[sheetIter->first][tempBlockId].rotation = 0.0f;
                BlockInfoList[sheetIter->first][tempBlockId].mirror = 1;
                widthOffset = abs(right/iconWidth);
                heightOffset = abs(top/iconHeight);
            }
            else if(modifier == 2)
            {
                BlockInfoList[sheetIter->first][tempBlockId].rotation = 0.0f;
                BlockInfoList[sheetIter->first][tempBlockId].mirror = 2;
                widthOffset = abs(left/iconWidth);
                heightOffset = abs(bottom/iconHeight);
            }
            else if(modifier == 3)
            {
                BlockInfoList[sheetIter->first][tempBlockId].rotation = 90.0f;
                BlockInfoList[sheetIter->first][tempBlockId].mirror = 0;
                widthOffset = abs(top/iconWidth);
                heightOffset = abs(right/iconHeight);
            }
            else if(modifier == 4)
            {
                BlockInfoList[sheetIter->first][tempBlockId].rotation = 270.0f;
                BlockInfoList[sheetIter->first][tempBlockId].mirror = 0;
                widthOffset = abs(bottom/iconWidth);
                heightOffset = abs(left/iconHeight);
            }
            else if(modifier == 5)
            {
                BlockInfoList[sheetIter->first][tempBlockId].rotation = 180.0f;
                BlockInfoList[sheetIter->first][tempBlockId].mirror = 0;
                widthOffset = abs(right/iconWidth);
                heightOffset = abs(bottom/iconHeight);
            }
            else if(modifier == 6)
            {
                BlockInfoList[sheetIter->first][tempBlockId].rotation = 270.0f;
                BlockInfoList[sheetIter->first][tempBlockId].mirror = 2;
                widthOffset = abs(top/iconWidth);
                heightOffset = abs(left/iconHeight);
            }
            else if(modifier == 7)
            {
                BlockInfoList[sheetIter->first][tempBlockId].rotation = 270.0f;
                BlockInfoList[sheetIter->first][tempBlockId].mirror = 1;
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
            
            //CString iconPath = ("2DIconsTemp/"+BlockInfoList[sheetIter->first][tempBlockId].type+"/"+BlockInfoList[sheetIter->first][tempBlockId].type+"."+BlockInfoList[sheetIter->first][tempBlockId].icon+".jpg").c_str();
            //LPWSTR lpszW = new WCHAR[255];
            //LPTSTR lpStr = iconPath.GetBuffer( iconPath.GetLength() );
            //int nLen = MultiByteToWideChar(CP_ACP, 0,lpStr, -1, NULL, NULL);
            //MultiByteToWideChar(CP_ACP, 0, lpStr, -1, lpszW, nLen);
            
            //GdiplusStartupInput gdiplusStartupInput;
            //ULONG_PTR gdiplusToken;
            //GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);
            //Image * image = new Image(lpszW);
			//Status fileStat = image->GetLastStatus();
			//if( fileStat != 0 )
            //{
            //    std::cout <<" Warning icon not found: "<< fileStat << std::endl;
            //}
            //float width = image->GetWidth();
            //float height = image->GetHeight();
            //GdiplusShutdown(gdiplusToken);
			
			float width = imageData[BlockInfoList[sheetIter->first][tempBlockId].type+"."+BlockInfoList[sheetIter->first][tempBlockId].icon+".JPG"].first;
            float height = imageData[BlockInfoList[sheetIter->first][tempBlockId].type+"."+BlockInfoList[sheetIter->first][tempBlockId].icon+".JPG"].second;
			tester4<<BlockInfoList[sheetIter->first][tempBlockId].type+"."+BlockInfoList[sheetIter->first][tempBlockId].icon+".JPG"<<": "<<width<<" "<<height<<std::endl;
            //iconLocations[ tempBlockId ] = std::pair< float, float >( scaledXCoords+200, scaledYCoords+200 );
            //iconLocations[ tempBlockId ] = std::pair< float, float >( scaledXCoords + 500 - (0.25*width), scaledYCoords + 500 - (0.25*height) );
            iconLocations[sheetIter->first][ tempBlockId ] = std::pair< float, float >( scaledXCoords - (width*widthOffset*BlockInfoList[sheetIter->first][tempBlockId].scale), scaledYCoords - (height*heightOffset*BlockInfoList[sheetIter->first][tempBlockId].scale) );
            count++;
        }
        std::cout<<"Finished Reading Block Info"<<std::endl;

        //locate minimum X - used for normalization
        float minX = 10000;
        float minY = 10000;
        std::map< std::string, std::pair< float, float > >::iterator iter;
        for (iter = iconLocations[sheetIter->first].begin(); iter != iconLocations[sheetIter->first].end(); iter++)
        {
            float currentX = iconLocations[sheetIter->first][ iter->first ].first;
            float currentY = iconLocations[sheetIter->first][ iter->first ].second;
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
                
                std::stringstream typeTokenizer(streamType);
                typeTokenizer >> discard;
                typeTokenizer >> xy.streamType;
                
                streamCoordList.push_back(xy); //add one streams values to vector      
                
				linkTypes[sheetIter->first][xy.streamId] = (xy.streamType);
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
                    linkPoints[sheetIter->first][xy.streamId].push_back( std::pair< float, float >( scaledX, scaledY ) );	
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
        //float normX = fabs(minX);
        //float normY = fabs(minY);
		float normX = minX;
		float normY = minY;
        tester2<<"NormX: "<<normX<<" NormY: "<<normY<<std::endl;
        tester2.close();
        //blocks
        std::ofstream tester3 ("tester3.txt");
        for(iter = iconLocations[sheetIter->first].begin(); iter != iconLocations[sheetIter->first].end(); iter++)
        {
            iconLocations[sheetIter->first][ iter->first ].first = iconLocations[sheetIter->first][iter->first].first - normX;
            iconLocations[sheetIter->first][ iter->first ].second = iconLocations[sheetIter->first][iter->first].second - normY;
            //iconLocations[ iter->first ].first = iconLocations[iter->first].first;
            //iconLocations[ iter->first ].second = iconLocations[iter->first].second;
            tester3<<iter->first<<": x: "<<iconLocations[sheetIter->first][ iter->first ].first<<" y: "<<iconLocations[sheetIter->first][ iter->first ].second<<std::endl;
        }
        tester3.close();

        //streams
        std::ofstream tester ("tester.txt");
        std::map< std::string, std::vector< std::pair< float, float > > >::iterator iter2;
        for(iter2 = linkPoints[sheetIter->first].begin(); iter2 != linkPoints[sheetIter->first].end(); iter2++)
        {
            tester<<iter2->first<<":";
            for(int element = 0; element < (int)linkPoints[sheetIter->first][ iter2->first ].size(); element++)
            {
                linkPoints[sheetIter->first][ iter2->first ][element].first = linkPoints[sheetIter->first][ iter2->first ][element].first - normX;
                linkPoints[sheetIter->first][ iter2->first ][element].second = linkPoints[sheetIter->first][ iter2->first ][element].second - normY;
                //linkPoints[ iter2->first ][element].first = linkPoints[ iter2->first ][element].first;
                //linkPoints[ iter2->first ][element].second = linkPoints[ iter2->first ][element].second;
                tester<<" x: "<< linkPoints[sheetIter->first][ iter2->first ][element].first<<" y: "<<(float)linkPoints[sheetIter->first][ iter2->first ][element].second;
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
        outFile << BlockInfoList[sheetIter->first].size()<<std::endl;
        while (count < (int)BlockInfoList[sheetIter->first].size())
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
    }
	tester4.close();
	//lutFile.close();
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
   
   //Obtain network chunk
   size_t networkBegin = networkData.find( std::string( "? SETUP MAIN ?" ) );
   //size_t networkBegin = 0;
   //size_t networkEnd = networkData.find( std::string( "? PROPERTIES MAIN ?" ) );
   size_t networkEnd = networkData.find( std::string( "GRAPHICS_BACKUP" ) );
   std::string network;
   network.append( networkData, networkBegin, (networkEnd - networkBegin) );
   
   size_t tagBegin = 0;
   size_t tagEnd = 0;
   
   // create the maps and network connectivity
   std::string blockName;
   std::string hierName = "0";
   std::string discard;
   //find first entry either ? or / or ;
   do
   {
	   //grab the first of all 3 types of indicators
	   size_t semi = network.find("\;", tagEnd);
	   size_t slash = network.find("\\", tagEnd);
	   size_t question = network.find("\?", tagEnd);

	   //check which one is first and go from there
	   if (semi < slash && semi < question)
	   {
		   tagBegin = semi;
		   tagEnd = network.find("\;", tagBegin + 1) + 1;
	   }
	   else if (slash < semi && slash < question)
	   {
		   tagBegin = slash;
		   tagEnd = network.find("\\", tagBegin + 1) + 1;
	   }
	   else if (question < slash && question < semi)
	   {
		   tagBegin = question;
		   tagEnd = network.find("\?", tagBegin + 1) + 1;
	   }

	   std::string blockData;
      if ( tagBegin != std::string::npos && tagEnd != std::string::npos)
      {
         blockData.append( network, tagBegin, (tagEnd - tagBegin) );
		 //std::cout<<blockData<<std::endl;
		 tagBegin = tagEnd + 1;
		 //if(blockData.find(std::string("COMMENTS")) == std::string::npos  &&
		//	blockData.find(std::string("FLOWSHEET")) == std::string::npos &&
		//	blockData.find(std::string("DEF-STREAM")) == std::string::npos&&
		//	blockData.find(std::string("CONNECT BLKID")) == std::string::npos)
		 //if(blockData.find(std::string("BLOCK BLKID")) != std::string::npos  ||
		 //	blockData.find(std::string("BLOCK HIERARCHY")) != std::string::npos)
		 //{
			 if(blockData.find(std::string("BLOCK HIERARCHY")) != std::string::npos)
			 {
				 StripCharacters( blockData, "?" );
				 StripCharacters( blockData, "\"" );
				 std::stringstream networkToks(blockData);
				 while(networkToks >> hierName);
			 }
			 else if(blockData.find(std::string("BLOCK BLKID")) != std::string::npos)
			 {
				 std::stringstream networkToks(blockData);
				 std::stringstream tempTokens(networkToks.str());
				 //networkToks.str().clear();
				 int toksCounter = 0;
				 while (tempTokens >> discard)
				 toksCounter++;
				 
				 std::vector< std::string >  vectorTokens;
				 for ( size_t i = 0; i < toksCounter; ++i )
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
					   //if(blockName.find(".") == std::string::npos)
					   //{
						   //models["Top_Sheet"][blockName] = index;
						   //models["0"][blockName] = tagBegin;
						   models[hierName][blockName] = tagBegin;
					   //}
					   //else
					   //{
						   //size_t  pos = blockName.find_last_of(".", 0);
						   //hierName = blockName.substr(0, pos);
						   //std::string tempBlockName = blockName.substr(pos+1, blockName.size());
						   //models[hierName][ tempBlockName ] = tagBegin;
					   //}
					}
					// this are the input links/streams that connect to this particular block
					else if ( (vectorTokens.at( i ) == std::string ( "=" )) && (vectorTokens.at( i - 1 ) == std::string( "IN" )) )
					{
					   std::string tempStrem = vectorTokens.at( i+=2 );
					   StripCharacters( tempStrem, "\"" );

					   inLinkToModel[hierName][ tempStrem ] = blockName;
					   ++i;
				       
					   while ( vectorTokens.at( i+1 ) != std::string( ")" ) )
					   {
						  tempStrem = vectorTokens.at( ++i );
						  StripCharacters( tempStrem, "\"" );
						  
						  inLinkToModel[hierName][ tempStrem ] = blockName;
						  ++i;
					   }
					}
					// this are the output links/streams that connect to this particular block
					else if ( (vectorTokens.at( i ) == std::string ( "=" )) && (vectorTokens.at( i - 1 ) == std::string( "OUT" )) )
					{
					   std::string tempStrem = vectorTokens.at( i+=2 );
					   StripCharacters( tempStrem, "\"" );
					   
					   outLinkToModel[hierName][ tempStrem ] = blockName;					   
					   ++i;
				       
					   while ( vectorTokens.at( i+1 ) != std::string( ")" ) )
					   {
						  tempStrem = vectorTokens.at( ++i );
						  StripCharacters( tempStrem, "\"" );
						  outLinkToModel[hierName][ tempStrem ] = blockName;
						  ++i;
					   }             
					}
				 }
			 }
			 else if(blockData.find(std::string("CONNECT BLKID")) != std::string::npos)
			 {
				 StripCharacters( blockData, "\\" );
				 std::stringstream networkToks(blockData);
				 std::string token;

				 //discard "CONNECT" "BLKID" "=" & "name"
				 for( size_t i = 0; i < 4; i++)
					 networkToks >> token;
				 
				 //populate vector with remaining tokens
				 std::vector< std::string >  vectorTokens;
				 while(networkToks >> token)
					 vectorTokens.push_back( token );

				 for ( size_t i = 0; i < vectorTokens.size(); ++i )
				 {
					// this are the input links/streams that connect to this particular block
					if ( (vectorTokens.at( i ) == std::string ( "=" )) && (vectorTokens.at( i - 1 ) == std::string( "IN" )) )
					{
						if(vectorTokens.at( i+2 ).find("\"") == std::string::npos)
						{
							inLinkToModel[hierName][ vectorTokens.at( i+=2 ) ] = blockName;
							++i;
						}
					}
					// this are the output links/streams that connect to this particular block
					else if ( (vectorTokens.at( i ) == std::string ( "=" )) && (vectorTokens.at( i - 1 ) == std::string( "OUT" )) )
					{
						if(vectorTokens.at( i+2 ).find("\"") == std::string::npos)
						{
							outLinkToModel[hierName][ vectorTokens.at( i+=2 ) ] = blockName;
							++i;
						}
					}
				 }
			 }
	   }
   }
   while( tagBegin < network.size()-1);
}
///////////////////////////////////////////////////////////
void BKPParser::CreateNetworkLinks( ves::open::xml::model::NetworkPtr subNetwork, std::string hierName )
{
	// remove duplicate points
	std::map< std::string, std::vector< std::pair< float, float > > >::iterator pointsIter;
	for ( pointsIter = linkPoints[hierName].begin(); pointsIter != linkPoints[hierName].end(); ++pointsIter )
	{
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
	for ( iter = inLinkToModel[hierName].begin(); iter != inLinkToModel[hierName].end(); ++iter )
	{
	  std::map< std::string, std::string >::iterator fromModel;
	  fromModel = outLinkToModel[hierName].find( iter->first );
	  if ( fromModel != outLinkToModel[hierName].end() )
	  {
		 //define link
		 // these are unique remember...
		 std::string toPortName = iter->first;
		 // these are unique remember...
		 std::string fromPortName = fromModel->first;

		 std::string toModelName;
		 std::string fromModelName;
		 if(hierName == "0")
		 {
			 toModelName = iter->second;
			 fromModelName = fromModel->second;
		 }
		 else
		 {
			 toModelName = hierName + "." + iter->second;
			 fromModelName = hierName + "." + fromModel->second;
		 }

		 int toPortId = counter++;
		 int fromPortId = counter++;
		 int toModelId = models[hierName][ iter->second ];
		 int fromModelId = models[hierName][ fromModel->second ];
		 streamPortIDS[ iter->first ] = std::pair< int, int >( toPortId, fromPortId );
         
		 //Now we create a link
		 //VE_XML::VE_Model::LinkWeakPtr xmlLink = subNetwork->GetLink( -1 );
		 ves::open::xml::model::LinkPtr xmlLink( new ves::open::xml::model::Link() );
		 xmlLink->GetFromModule()->SetData( fromModelName, static_cast< long int >( fromModelId ) );
		 xmlLink->GetToModule()->SetData( toModelName, static_cast< long int >( toModelId ) );
		 
		 //if (hierName == "0")
			 xmlLink->SetLinkName(iter->first);
			 xmlLink->SetLinkType(linkTypes[hierName][iter->first]);
		 //else
			 //xmlLink->SetLinkName(hierName + "." + iter->first);
		 
		 *(xmlLink->GetFromPort()) = static_cast< long int >( fromPortId );
		 *(xmlLink->GetToPort()) = static_cast< long int >( toPortId );

		 for ( size_t j = linkPoints[hierName][ fromPortName ].size(); j > 0 ; --j )
		 {
			// I am not sure why we need to reverse the points but we do
			xmlLink->GetLinkPoint( linkPoints[hierName][ fromPortName ].size() - j )->SetPoint( linkPoints[hierName][ fromPortName ].at( j - 1 ) );
		 }
		 subNetwork->AddLink( xmlLink );
	  }
   }
}
///////////////////////////////////////////////////////////////////////
std::string BKPParser::CreateNetwork( void )
{
   // then create the appropriate models
   // then put them all together and for a network string
   // Here we wshould loop over all of the following
   std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
   ves::open::xml::model::NetworkPtr mainNetwork( new ves::open::xml::model::Network() );
   ves::open::xml::model::SystemPtr veSystem( new ves::open::xml::model::System() );
   
   nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( veSystem, "veSystem" ) );
   
   // create default state info section
   mainNetwork->GetDataValuePair( -1 )->SetData( "m_xUserScale", 1.0 );
   mainNetwork->GetDataValuePair( -1 )->SetData( "m_yUserScale", 1.0 );
   mainNetwork->GetDataValuePair( -1 )->SetData( "nPixX", static_cast< long int >( 20 ) );
   mainNetwork->GetDataValuePair( -1 )->SetData( "nPixY", static_cast< long int >( 20 ) );
   mainNetwork->GetDataValuePair( -1 )->SetData( "nUnitX", static_cast< long int >( 200 ) );
   mainNetwork->GetDataValuePair( -1 )->SetData( "nUnitY", static_cast< long int >( 200 ) );
   veSystem->AddNetwork(mainNetwork);

   CreateNetworkLinks(mainNetwork, "0");

   // Loop over the top networks blocks
   std::map< std::string, int >::iterator blockIter;
   for ( blockIter = models["0"].begin(); blockIter != models["0"].end(); ++blockIter )
   {
      ves::open::xml::model::ModelPtr tempModel( new ves::open::xml::model::Model() );
	  tempModel->SetModelID( blockIter->second );
	  tempModel->SetModelName( blockIter->first );
	  tempModel->SetVendorName( "ASPENUNIT" );
	  tempModel->SetIconFilename(BlockInfoList["0"][blockIter->first].type+"/"+BlockInfoList["0"][blockIter->first].type+"."+BlockInfoList["0"][blockIter->first].icon);
	  tempModel->SetIconRotation(BlockInfoList["0"][blockIter->first].rotation);
	  tempModel->SetIconScale(BlockInfoList["0"][blockIter->first].scale);
	  tempModel->SetIconMirror(BlockInfoList["0"][blockIter->first].mirror);
	  tempModel->GetIconLocation()->SetPoint( std::pair< double, double >( iconLocations["0"][ blockIter->first ].first, iconLocations["0"][ blockIter->first ].second ) );
      
	  double minX = iconLocations["0"][ blockIter->first ].first;
	  double minY = iconLocations["0"][ blockIter->first ].second;

      // input ports
	  std::map< std::string, std::string >::iterator streamIter;
      for ( streamIter = inLinkToModel["0"].begin(); streamIter != inLinkToModel["0"].end(); ++streamIter )
      {
         if ( streamIter->second == blockIter->first )
         {
			 ves::open::xml::model::PortPtr tempPort = tempModel->GetPort(-1);
            // inputs are to ports
            tempPort->SetPortNumber( streamPortIDS[ streamIter->first ].first );
            tempPort->SetModelName( streamIter->first );
            tempPort->SetDataFlowDirection( std::string( "input" ) );
			tempPort->GetPortLocation()->SetPoint( std::pair< double, double >( (linkPoints["0"][tempPort->GetModelName()][0].first - minX ), (linkPoints["0"][tempPort->GetModelName()][0].second - minY ) ) );
		 }
      }
      // output ports
      for ( streamIter = outLinkToModel["0"].begin(); streamIter != outLinkToModel["0"].end(); ++streamIter )
      {
         if ( streamIter->second == blockIter->first )
         {
            ves::open::xml::model::PortPtr tempPort = tempModel->GetPort(-1);
            // outputs are from ports
            tempPort->SetPortNumber( streamPortIDS[ streamIter->first ].second );
            tempPort->SetModelName( streamIter->first );
            tempPort->SetDataFlowDirection( std::string( "output" ) );
			tempPort->GetPortLocation()->SetPoint( std::pair< double, double >( (linkPoints["0"][tempPort->GetModelName()][linkPoints["0"][tempPort->GetModelName()].size()-1].first - minX ), (linkPoints["0"][tempPort->GetModelName()][linkPoints["0"][tempPort->GetModelName()].size()-1].second - minY ) ) );
         }
      }

	  //acquire sublinks
	  //VE_XML::VE_Model::NetworkPtr subnet = new VE_XML::VE_Model::Network();
	  //CreateNetworkLinks(subnet, tempModel->GetModelName());

	  //recursively parse subsystems of each block
	  if(tempModel->GetIconFilename().find("HIERARCHY") != std::string::npos)
		  ParseSubSystem(tempModel, blockIter->first);

	  //create subsystem
	  //VE_XML::VE_Model::SystemWeakPtr subSystem = new VE_XML::VE_Model::System();
	  //subSystem->AddNetwork(subnet);
	  //tempModel->SetSubSystem(subSystem);

	  //attach model to top system
	  veSystem->AddModel(tempModel);
   }
   std::string fileName( "returnString" );
   ves::open::xml::XMLReaderWriter netowrkWriter;
   netowrkWriter.UseStandaloneDOMDocumentManager();
   netowrkWriter.WriteXMLDocument( nodes, fileName, "Network" );
   return fileName;
}

////////////////////////////////////////////////////////////////////////////////
void BKPParser::ParseSubSystem(ves::open::xml::model::ModelPtr model, std::string networkName)
{
	ves::open::xml::model::SystemPtr subSystem( new ves::open::xml::model::System() );
	ves::open::xml::model::NetworkPtr subNetwork( new ves::open::xml::model::Network() );
	subSystem->AddNetwork(subNetwork);

	CreateNetworkLinks(subNetwork, networkName);

	// Loop over the top networks blocks
	std::map< std::string, int >::iterator blockIter;
	for ( blockIter = models[networkName].begin(); blockIter != models[networkName].end(); ++blockIter )
	{
		ves::open::xml::model::ModelPtr tempModel( new ves::open::xml::model::Model() );
		tempModel->SetModelID( blockIter->second );
		tempModel->SetModelName( blockIter->first );
		tempModel->SetVendorName( "ASPENUNIT" );
		tempModel->SetIconFilename(BlockInfoList[networkName][blockIter->first].type+"/"+BlockInfoList[networkName][blockIter->first].type+"."+BlockInfoList[networkName][blockIter->first].icon);
		tempModel->SetIconRotation(BlockInfoList[networkName][blockIter->first].rotation);
		tempModel->SetIconScale(BlockInfoList[networkName][blockIter->first].scale);
		tempModel->SetIconMirror(BlockInfoList[networkName][blockIter->first].mirror);
		tempModel->GetIconLocation()->SetPoint( std::pair< double, double >( iconLocations[networkName][ blockIter->first ].first, iconLocations[networkName][ blockIter->first ].second ) );

		double minX = iconLocations[networkName][ blockIter->first ].first;
		double minY = iconLocations[networkName][ blockIter->first ].second;

		// input ports
		std::map< std::string, std::string >::iterator streamIter;
		for ( streamIter = inLinkToModel[networkName].begin(); streamIter != inLinkToModel[networkName].end(); ++streamIter )
		{
		 if ( streamIter->second == blockIter->first )
		 {
            ves::open::xml::model::PortPtr tempPort = tempModel->GetPort(-1);
			// inputs are to ports
			tempPort->SetPortNumber( streamPortIDS[ streamIter->first ].first );
			tempPort->SetModelName( streamIter->first );
			tempPort->SetDataFlowDirection( std::string( "input" ) );
			tempPort->GetPortLocation()->SetPoint( std::pair< double, double >( (linkPoints[networkName][tempPort->GetModelName()][0].first - minX ), (linkPoints[networkName][tempPort->GetModelName()][0].second - minY ) ) );
		 }
		}
		// output ports
		for ( streamIter = outLinkToModel[networkName].begin(); streamIter != outLinkToModel[networkName].end(); ++streamIter )
		{
		 if ( streamIter->second == blockIter->first )
		 {
            ves::open::xml::model::PortPtr tempPort = tempModel->GetPort(-1);
			// outputs are from ports
			tempPort->SetPortNumber( streamPortIDS[ streamIter->first ].second );
			tempPort->SetModelName( streamIter->first );
			tempPort->SetDataFlowDirection( std::string( "output" ) );
			tempPort->GetPortLocation()->SetPoint( std::pair< double, double >( (linkPoints[networkName][tempPort->GetModelName()][linkPoints[networkName][tempPort->GetModelName()].size()-1].first - minX ), (linkPoints[networkName][tempPort->GetModelName()][linkPoints[networkName][tempPort->GetModelName()].size()-1].second - minY ) ) );
		 }
		}

	  //recursively parse subsystems of each block
	  if(tempModel->GetIconFilename().find("HIERARCHY") != std::string::npos)
		  ParseSubSystem(tempModel, networkName + "." + blockIter->first);

	  //attach model to top system
	  subSystem->AddModel(tempModel);
	}
	model->SetSubSystem(subSystem);
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
	
    ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
	std::vector<std::string> paramList;
	//input variables;
	params->SetCommandName((modname+"InputParams").c_str());
	
	for (i = 0; i < (int)cur_block.getNumInputVar(); i++)
	{	
        paramList.push_back((char*)LPCTSTR(cur_block.getInputVarName(i)));
    }

	ves::open::xml::DataValuePairPtr inpParams( new ves::open::xml::DataValuePair() );
	inpParams->SetData("params",paramList);
    params->AddDataValuePair( inpParams );
    
	std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
	nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, 
        std::string >( params, "vecommand" ) );

	ves::open::xml::XMLReaderWriter commandWriter;
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
	
    ves::open::xml::CommandPtr properties( new ves::open::xml::Command() );
	const int propSize=23;
	properties->SetCommandName((modname+paramName).c_str());
	std::cout<<(modname+paramName).c_str()<<std::endl;

	ves::open::xml::DataValuePairPtr Props[propSize];
	for (j=0; j<propSize; j++)
	{
		Props[j] = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
		Props[j]->SetDataType("STRING");
		properties->AddDataValuePair( Props[j] );
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

	std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;

	nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( properties, "vecommand" ) );

	ves::open::xml::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}

std::string BKPParser::GetOutputModuleParams(std::string modname)
{
	CASI::CASIObj cur_block= aspendoc->getBlockByName(CString(modname.c_str()));
	int i;
	
    ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
	std::vector<std::string> paramList;
	//input variables;
	params->SetCommandName((modname+"OutputParams").c_str());
	
	for (i = 0; i < (int)cur_block.getNumOutputVar(); i++)
		paramList.push_back((char*)LPCTSTR(cur_block.getOutputVarName(i)));

	ves::open::xml::DataValuePairPtr inpParams( new ves::open::xml::DataValuePair() );
	inpParams->SetData("params",paramList);
    params->AddDataValuePair( inpParams );

	std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
	nodes.push_back( 
                  std::pair< ves::open::xml::XMLObjectPtr, std::string >( params, "vecommand" ) 
                     );

	ves::open::xml::XMLReaderWriter commandWriter;
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
	
    ves::open::xml::CommandPtr properties( new ves::open::xml::Command() );
	const int propSize=23;
	properties->SetCommandName((modname+paramName).c_str());
	std::cout<<(modname+paramName).c_str()<<std::endl;

	ves::open::xml::DataValuePairPtr Props[propSize];
	for (j=0; j<propSize; j++)
	{
		Props[j] = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
		Props[j]->SetDataType("STRING");
		properties->AddDataValuePair( Props[j] );
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

	std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;

	nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( properties, "vecommand" ) );

	ves::open::xml::XMLReaderWriter commandWriter;
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
	
	ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
	std::vector<std::string> paramList;
	//input variables;
	params->SetCommandName((modname+"InputParams").c_str());
	int test = (int)cur_stream.getNumInputVar();
	for (i = 0; i < (int)cur_stream.getNumInputVar(); i++)
		//paramList.push_back((char*)LPCTSTR(cur_stream.getStreamCompName(i)));
		paramList.push_back((char*)LPCTSTR(cur_stream.getInputVarName(i)));

	ves::open::xml::DataValuePairPtr inpParams( new ves::open::xml::DataValuePair() );
	inpParams->SetData("params",paramList);
    params->AddDataValuePair( inpParams );

	std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
	nodes.push_back( 
                  std::pair< ves::open::xml::XMLObjectPtr, std::string >( params, "vecommand" ) 
                     );

	ves::open::xml::XMLReaderWriter commandWriter;
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
	
    ves::open::xml::CommandPtr properties( new ves::open::xml::Command() );
	const int propSize=23;
	properties->SetCommandName((modname+paramName).c_str());
	std::cout<<(modname+paramName).c_str()<<std::endl;

	ves::open::xml::DataValuePairPtr Props[propSize];
	for (j=0; j<propSize; j++)
	{
		Props[j] = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
		Props[j]->SetDataType("STRING");
		properties->AddDataValuePair( Props[j] );
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

	std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;

	nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( properties, "vecommand" ) );

	ves::open::xml::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}

std::string BKPParser::GetStreamOutputModuleParams(std::string modname)
{
	CASI::CASIObj cur_stream= aspendoc->getStreamByName(CString(modname.c_str()));
	int i;
	
    ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
	std::vector<std::string> paramList;
	//input variables;
	params->SetCommandName((modname+"OutputParams").c_str());
	
	for (i = 0; i < (int)cur_stream.getNumOutputVar(); i++)
		paramList.push_back((char*)LPCTSTR(cur_stream.getOutputVarName(i)));

	ves::open::xml::DataValuePairPtr inpParams( new ves::open::xml::DataValuePair() );
	inpParams->SetData("params",paramList);
    params->AddDataValuePair( inpParams );

	std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
	nodes.push_back( 
                  std::pair< ves::open::xml::XMLObjectPtr, std::string >( params, "vecommand" ) 
                     );

	ves::open::xml::XMLReaderWriter commandWriter;
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
	
    ves::open::xml::CommandPtr properties( new ves::open::xml::Command() );
	const int propSize=23;
	properties->SetCommandName((modname+paramName).c_str());
	std::cout<<(modname+paramName).c_str()<<std::endl;

	ves::open::xml::DataValuePairPtr Props[propSize];
	for (j=0; j<propSize; j++)
	{
		Props[j] = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
		Props[j]->SetDataType("STRING");
		properties->AddDataValuePair( Props[j] );
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

	std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;

	nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( properties, "vecommand" ) );

	ves::open::xml::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}