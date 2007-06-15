/*************** <auto-copyright.pl BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> **************/
#include <iostream>
#include <fstream>
#include "bkpparser.h"
#include "StringTokenizer.h"

BKPParser::BKPParser()
{
}

BKPParser::~BKPParser()
{
}

void BKPParser::openFile(char * file)
{
	ParseFile(file);	
}

int BKPParser::getNumComponents()
{
	return BlockInfoList.size(); //vectors are all same length
}

std::string BKPParser::getBlockType(int num)
{
	return BlockInfoList[num].type;
}

std::string BKPParser::getBlockIcon(int num)
{
	return BlockInfoList[num].icon;
}

std::string BKPParser::getBlockID(int num)
{
	return BlockInfoList[num].id;
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
	return streamCoordList[streamIndex].value[coordIndex].x;
}

float BKPParser::getStreamYCoord(int streamIndex, int coordIndex)
{
	return streamCoordList[streamIndex].value[coordIndex].y;
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

void BKPParser::ParseFile(char * bkpFile)
{
	//Open file streams	
	std::ifstream inFile (bkpFile);
	std::ofstream outFile("log.txt");
	
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
	
	//dump first 27 lines
	//int i = 0;
	//while (i <27)
	//{
	//	getline(inFile, temp);
	//	i++;
	//}
	//cout << "Skip first 27 lines."<<endl;
	
	while(	temp.compare(0 , 7, "NumLibs", 0, 7))
	{
		getline(inFile, temp);
	}

	StringTokenizer numLibToken = StringTokenizer(temp, " ");
	numLibToken.nextToken();//Dump "NumLibs"
	numLibToken.nextToken();//Dump "="
	int numLibs = numLibToken.nextIntToken(); //get int of # libs
	int i=0;
	while(i<numLibs)
	{
		getline(inFile, temp);
		i++;
	}	
	
	getline(inFile, temp);
	StringTokenizer numCatToken = StringTokenizer(temp, " ");
	numCatToken.nextToken();//Dump "NumLibs"
	numCatToken.nextToken();//Dump "="
	int numCats = numCatToken.nextIntToken(); //get int of # libs
	
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
	while(count < numComponents)
	{
		getline(inFile, compVer);
		getline(inFile, compID);
		getline(inFile, compName);
		getline(inFile, compLib);
		getline(inFile, compLibName);

		//create a vector containg ID and type of block
		StringTokenizer Tokenizer = StringTokenizer(compID, " ");
		tempBlockInfo.id=Tokenizer.nextToken("\r");	
		Tokenizer = StringTokenizer(compName, " ");
		tempBlockInfo.type = Tokenizer.nextToken("\r");
		BlockInfoList.push_back(tempBlockInfo);	
		count++;
	}
	std::cout<<"Aqcuired type/id list"<<std::endl;

	//find the graphics section
	while(temp.compare(0, 16, " GRAPHICS_BACKUP", 0, 16)!= 0 && !inFile.eof())
	{
		getline(inFile, temp);
	}
	std::cout<<"Found Graphics Section"<<std::endl;

	//find first block
	while(temp.compare(0, 5, "BLOCK", 0, 5)!= 0 && !inFile.eof())
	{
		getline(inFile, temp);
	}
	std::cout<<"First Block Found"<<std::endl;
	
	//Read graphic blocks
	count =0;
	std::string id, version, icon, flag, section, at, labelAt, scaleMod, annotation;
	while(count < numComponents)
	{
		getline(inFile, id);
		getline(inFile, version);
		getline(inFile, icon);
		StringTokenizer iconTokenizer = StringTokenizer(icon, "\"");
		iconTokenizer.nextToken();
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

		getline(inFile, temp); //dump next block header Should BE CHANGED - when next entry is important
		
		//parse id to use for searching
		StringTokenizer idTokenizer = StringTokenizer(id, " ");
		idTokenizer.nextToken(" ");
		std::string tempBlockId =idTokenizer.nextToken("\r");
		BlockInfo tempBlockInfo_2;
		//sort the block id/type vector
		int entryIncr = 0;
		bool entryFound = false;
		while((entryIncr < BlockInfoList.size()) && (!entryFound) )
		{
			//find the entry;remove original occurrence;push it to the back of vector
			if(tempBlockId==BlockInfoList[entryIncr].id)
			{
				//tempBlockInfo_2 = BlockInfoList[entryIncr];	
				//add icon value to block type
				//allows for all variations of 3d representations of unit operations
				//tempBlockInfo_2.type +="."+iconTokenizer.nextToken();
				//BlockInfoList.erase(BlockInfoList.begin()+entryIncr);
				//BlockInfoList.push_back(tempBlockInfo_2);
				BlockInfoList[entryIncr].icon = iconTokenizer.nextToken();
				entryFound = true;
			}
			entryIncr++;
		} 

		//parse location to create coordinates for blocks
		StringTokenizer atTokenizer = StringTokenizer(at, " ");
		atTokenizer.nextToken(" ");
		xCoords.push_back((float)atTokenizer.nextFloatToken());
		yCoords.push_back((float)atTokenizer.nextFloatToken());

		count++;
	}
	std::cout<<"Finished Reading Block Info"<<std::endl;

	//
	//Stream Info
	//

	std::cout<<"Begin Reading Streams"<<std::endl;
	//Gather stream information
	std::string streamId, streamVersion, streamFlag, streamType, coordinates, tempR, tempR2;
	bool newStream = true, routeOne = false;;
	streamCoords tempCoords;
	int routeCount = 0;
	
	//contiously read all stream info to the legend or viewport entry
	while(temp.compare(0, 8, "VIEWPORT", 0, 8)!= 0 && temp.compare(0, 6, "LEGEND", 0, 6)!= 0 && !inFile.eof())
	{
		if(temp.compare(0, 6, "STREAM", 0, 6)== 0)//find "STREAM" entry
		{
			getline(inFile, streamId);
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
						if(routeCount == 1)
						{
							StringTokenizer streamTokenizer = StringTokenizer(temp, " ");
							tempR=streamTokenizer.nextToken(" ");
							tempR2=streamTokenizer.nextToken(" ");	
							tempCoords.x=(float)streamTokenizer.nextFloatToken();
							tempCoords.y=(float)streamTokenizer.nextFloatToken();
							xy.value.push_back(tempCoords);
							routeOne = true;
						}
						else if(routeCount == 2)
						{
							StringTokenizer streamTokenizer = StringTokenizer(temp, " ");
							tempR=streamTokenizer.nextToken(" ");
							tempR2=streamTokenizer.nextToken(" ");	
							tempCoords.x=(float)streamTokenizer.nextFloatToken();
							tempCoords.y=(float)streamTokenizer.nextFloatToken();
							tempXY.value.push_back(tempCoords);
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
					while(tempCount < tempXY.value.size())
					{
						xy.value.insert(xy.value.begin(), tempXY.value[tempCount]);
						tempCount++;
					}
					StringTokenizer idTokenizer = StringTokenizer(streamId, " ");
					idTokenizer.nextToken();	
					xy.streamId=idTokenizer.nextToken("\r");	
					StringTokenizer typeTokenizer = StringTokenizer(streamType, " ");
					typeTokenizer.nextToken();	
					xy.streamType=typeTokenizer.nextIntToken();
					streamCoordList.push_back(xy); //add one streams values to vector
					xy.value.erase(xy.value.begin(),xy.value.end() );//empty temporary vector
					tempXY.value.erase(tempXY.value.begin(),tempXY.value.end() );//empty temporary vector
				
		}
		else 
			getline(inFile, temp);
	}
	std::cout<<"Finished Reading Streams"<<std::endl;
	
	//
	// Log
	//

	//create a log file
	std::cout<<"Writing log."<<std::endl;
	count = 0;
	int streamCount = 0;
	outFile << BlockInfoList.size()<<std::endl;
	while (count<BlockInfoList.size())
	{
		outFile << BlockInfoList[count].type;
		outFile << "\t";
		outFile << BlockInfoList[count].icon;
		outFile << "\t";
		outFile << BlockInfoList[count].id;
		outFile << "\t";
		outFile << xCoords[count];
		outFile << "\t";
		outFile << yCoords[count];
		outFile << "\n";
		count ++;
	}
	count=0;
	outFile << streamCoordList.size()<<std::endl;
	while(streamCount < streamCoordList.size())
	{
		outFile<<streamCoordList[streamCount].value.size()<<std::endl;
		while(count < streamCoordList[streamCount].value.size())
		{	
			outFile << streamCoordList[streamCount].value[count].x;
			outFile << "\t";
			outFile << streamCoordList[streamCount].value[count].y;
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
