#include <iostream>
#include <fstream>
#include <algorithm>
#include "bkpparser.h"
#include "StringTokenizer.h"
#include "VE_Open/XML/Model/Network.h"
#include "VE_Open/XML/Model/Link.h"
#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/Model/Point.h"
#include "VE_Open/XML/Model/Port.h"

BKPParser::BKPParser()
{
   veNetwork = new VE_Model::Network();
}

BKPParser::~BKPParser()
{
}

void BKPParser::openFile(const char * file)
{
	ParseFile(file);	
	aspendoc.open(file);
}

int BKPParser::getNumComponents()
{
	return BlockInfoList.size(); //vectors are all same length
}

std::string BKPParser::getBlockType(int num)
{
	return BlockInfoList[num].type;
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

   //The following block contains the network data
   std::streampos beforeNetwork;
   beforeNetwork = inFile.tellg();

	//find the graphics section
	while(temp.compare(0, 16, " GRAPHICS_BACKUP", 0, 16)!= 0 && !inFile.eof())
	{
		getline(inFile, temp);
	}
	std::cout<<"Found Graphics Section"<<std::endl;
   //Now we have passed the network data so record it
   std::streampos afterNetwork;
   afterNetwork = inFile.tellg();
   //go back to the beginning of the network
   inFile.seekg( beforeNetwork );
   // allocate memory:
   char* buffer = new char [afterNetwork - beforeNetwork];
   // read data as a block:
   inFile.read( buffer, (afterNetwork - beforeNetwork) );
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
				tempBlockInfo_2 = BlockInfoList[entryIncr];	
				//add icon value to block type
				//allows for all variations of 3d representations of unit operations
				tempBlockInfo_2.type +="."+iconTokenizer.nextToken();
				BlockInfoList.erase(BlockInfoList.begin()+entryIncr);
				BlockInfoList.push_back(tempBlockInfo_2);
				entryFound = true;
			}
			entryIncr++;
		} 

		//parse location to create coordinates for blocks
		StringTokenizer atTokenizer = StringTokenizer(at, " ");
		atTokenizer.nextToken(" ");
		xCoords.push_back((float)atTokenizer.nextFloatToken());
		yCoords.push_back((float)atTokenizer.nextFloatToken());
      iconLocations[ tempBlockId ] = std::pair< float, float >( xCoords.back() + 20, yCoords.back() + 20 );
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
	std::pair< float, float > tempCoords;
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
							tempCoords.first=(float)streamTokenizer.nextFloatToken();
							tempCoords.second=(float)streamTokenizer.nextFloatToken();
							xy.value.push_back(tempCoords);
							routeOne = true;
						}
						else if(routeCount == 2)
						{
							StringTokenizer streamTokenizer = StringTokenizer(temp, " ");
							tempR=streamTokenizer.nextToken(" ");
							tempR2=streamTokenizer.nextToken(" ");	
							tempCoords.first=(float)streamTokenizer.nextFloatToken();
							tempCoords.second=(float)streamTokenizer.nextFloatToken();
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
         //Create map of stream names to points
         std::vector< std::pair< unsigned int, unsigned int > > tempPointVector;
         for ( size_t k = 0; k < xy.value.size(); ++k )
         {
            unsigned int xLoc = static_cast< unsigned int >( xy.value.at( k ).first + 20 );
            unsigned int yLoc = static_cast< unsigned int >( xy.value.at( k ).second + 20 );
            tempPointVector.push_back( std::pair< unsigned int, unsigned int >( xLoc, yLoc ) );
         }
         // add converted points for wx
         linkPoints[ xy.streamId ] = tempPointVector;
         //std::cout << xy.streamId << " : " << xy.value.size() << std::endl;				
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
   /*for ( size_t index = 0; index < networkData.length(); )
   {
      index = networkData.find( "\n", index );
      if ( index != std::string::npos )
         networkData.erase( index, 1 );
   }*/
   StripCharacters( networkData, "\n" );

   // strip the <cr>
   /*for ( size_t index = 0; index < networkData.length(); )
   {
      index = networkData.find( "\r", index );
      if ( index != std::string::npos )
         networkData.erase( index, 1 );
   }*/
   StripCharacters( networkData, "\r" );

   /// Add code the reads the block network info
   /// Search for "? FLOWSHEET GLOBAL ?"
   /// then grab all sections that start with \ and end with \ tempe
   size_t networkBegin = networkData.find( std::string( "? FLOWSHEET GLOBAL ?" ) );
   //std::cout << networkBegin << std::endl;
   size_t networkEnd = networkData.find( std::string( "? PROPERTIES MAIN ?" ) );
   //std::cout << networkEnd << std::endl;
   std::string network;
   network.append( networkData, networkBegin, (networkEnd - networkBegin) );
   
   size_t tag = 0;
   size_t index = 0;
   // create the maps and network connectivity
   do
   {
      tag = network.find( std::string( "\\" ), index );
      std::string blockData;
      if ( tag != std::string::npos )
      {
         blockData.append( network, index, (tag - index) );
         index = tag + 1;
         std::cout << blockData << std::endl;
         StringTokenizer networkToks( blockData, " " );
         int toksCounter = networkToks.countTokens();
         std::vector< std::string >  vectorTokens;
         for ( int i = 0; i < toksCounter; ++i )
         {
            std::string token = networkToks.nextToken();
            vectorTokens.push_back( token );
         }
         
         std::string blockName;
         // Now parse the vector of tokens...
         for ( size_t i = 0; i < vectorTokens.size(); ++i )
         {
            // This is the block names
            if ( (vectorTokens.at( i ) == std::string( "=" )) && (vectorTokens.at( i - 1 ) == std::string( "BLKID" )) )
            {
               blockName = vectorTokens.at( ++i );
               StripCharacters( blockName, "\"" );
               models[ blockName ] = index;
               std::cout << blockName << " : " << index << std::endl;
            }
            // this are the input links/streams that connect to this particular block
            else if ( (vectorTokens.at( i ) == std::string ( "=" )) && (vectorTokens.at( i - 1 ) == std::string( "IN" )) )
            {
               std::string tempStrem = vectorTokens.at( i+=2 );
               StripCharacters( tempStrem, "\"" );
               inLinkToModel[ tempStrem ] = blockName;
               std::cout << tempStrem << std::endl;
               ++i;
               
               while ( vectorTokens.at( i+1 ) != std::string( ")" ) )
               {
                  tempStrem = vectorTokens.at( ++i );
                  StripCharacters( tempStrem, "\"" );
                  inLinkToModel[ tempStrem ] = blockName;
                  std::cout << tempStrem << " : " << inLinkToModel[ tempStrem ] << std::endl;
                  ++i;
               }
            }
            // this are the output links/streams that connect to this particular block
            else if ( (vectorTokens.at( i ) == std::string ( "=" )) && (vectorTokens.at( i - 1 ) == std::string( "OUT" )) )
            {
               std::string tempStrem = vectorTokens.at( i+=2 );
               StripCharacters( tempStrem, "\"" );
               outLinkToModel[ tempStrem ] = blockName;
               //std::cout << vectorTokens.at( i+=2 ) << std::endl;
               ++i;
               
               while ( vectorTokens.at( i+1 ) != std::string( ")" ) )
               {
                  tempStrem = vectorTokens.at( ++i );
                  StripCharacters( tempStrem, "\"" );
                  outLinkToModel[ tempStrem ] = blockName;
                  //std::cout << vectorTokens.at( ++i ) << std::endl;
                  ++i;
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
   std::map< std::string, std::vector< std::pair< unsigned int, unsigned int > > >::iterator pointsIter;
   for ( pointsIter = linkPoints.begin(); pointsIter != linkPoints.end(); ++pointsIter )
   {
      std::vector< std::pair< unsigned int, unsigned int > > tempPoints;
      tempPoints = pointsIter->second;
      std::vector< std::pair< unsigned int, unsigned int > >::iterator pairIter;
      for ( pairIter = tempPoints.begin(); pairIter != tempPoints.end(); )
      {
         // need to remove duplicate points
         //pairIter++;
         std::vector< std::pair< unsigned int, unsigned int > >::iterator tempPairIter;
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
         /*std::vector< std::string > portNames;
         std::vector< std::string >::iterator nameIter;
         //nameIter = std::find( portNames.begin(), portNames.end(), std::string("here") );
         int portNumber = -1;
         for ( size_t i = 0; i < portNames.size(); ++i )
         {
            if ( std::string("here")  == portNames.at( i ) )
            {
               portNumber = i;
               break;
            }
         }
         // i  is then the port number*/
         //Now we create a link
            VE_Model::Link* xmlLink = veNetwork->GetLink( -1 );
            //xmlLink->GetFromPort()->SetData( modules[ links[i].GetFromModule() ].GetPlugin()->GetModelName(), links[i].GetFromPort() );
            //xmlLink->GetToPort()->SetData( modules[ links[i].GetToModule() ].pl_mod->GetModelName(), links[i].GetToPort() );
            xmlLink->GetFromModule()->SetData( fromModelName, static_cast< long int >( fromModelId ) );
            xmlLink->GetToModule()->SetData( toModelName, static_cast< long int >( toModelId ) );
            *(xmlLink->GetFromPort()) = static_cast< long int >( fromPortId );
            *(xmlLink->GetToPort()) = static_cast< long int >( toPortId );

            //Try to store link cons,
            //link cons are (x,y) wxpoint
            //here I store x in one vector and y in the other
            for ( size_t j = 0; j < linkPoints[ fromPortName ].size(); ++j )
	         {
               xmlLink->GetLinkPoint( j )->SetPoint( linkPoints[ fromPortName ].at( j ) );
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
   veNetwork->GetDataValuePair( -1 )->SetData( "nPixX", static_cast< long int >( 1 ) );
   veNetwork->GetDataValuePair( -1 )->SetData( "nPixY", static_cast< long int >( 1 ) );
   veNetwork->GetDataValuePair( -1 )->SetData( "nUnitX", static_cast< long int >( 1 ) );
   veNetwork->GetDataValuePair( -1 )->SetData( "nUnitY", static_cast< long int >( 1 ) );

   // Create links section
   CreateNetworkLinks();

   //  Models
   std::map< std::string, int >::iterator iter;
   for ( iter=models.begin(); iter!=models.end(); ++iter )
   {
      VE_Model::Model* tempModel = new VE_Model::Model();
      tempModel->SetModelID( iter->second );
      tempModel->SetModelName( iter->first );
      tempModel->GetIconLocation()->SetPoint( std::pair< unsigned int, unsigned int >( iconLocations[ iter->first ].first, iconLocations[ iter->first ].second ) );
      std::map< std::string, std::string >::iterator iterStreams;
	  
	  CASI::CASIObj cur_block = aspendoc.getBlockByName(iter->first.c_str());
	  unsigned int i;
	  //input variables;
	  for (i=0; i<cur_block.getNumInputVar(); i++)
	  {
		VE_XML::DataValuePair* tempInput=tempModel->GetInput(-1);
		tempInput->SetDataType("STRING");
		
		tempInput->SetDataName((char*)LPCTSTR(cur_block.getInputVarName(i)));
		CASI::Variable tempvar = cur_block.getInputVarByIndex(i);
		tempInput->SetDataString((char*)LPCTSTR(tempvar.getValue()));
	  }
	  //Result variables
	  for (i=0; i<cur_block.getNumInputVar(); i++)
	  {
		VE_XML::DataValuePair* tempResult=tempModel->GetResult(-1);
		tempResult->SetDataType("STRING");
		tempResult->SetDataName((char*)LPCTSTR(cur_block.getInputVarName(i)));
		CASI::Variable tempvar = cur_block.getOutputVarByIndex(i);
		tempResult->SetDataString((char*)LPCTSTR(tempvar.getValue()));
	  }
      // input ports
      for ( iterStreams = inLinkToModel.begin(); iterStreams != inLinkToModel.end(); ++iterStreams )
      {
         if ( iterStreams->second == iter->first )
         {
            VE_Model::Port* tempPort = tempModel->GetPort(-1);
            // inputs are to ports
            tempPort->SetPortNumber( streamPortIDS[ iterStreams->first ].second );
            tempPort->SetModelName( iterStreams->first );
            tempPort->SetDataFlowDirection( std::string( "input" ) );
            tempPort->GetPortLocation()->SetPoint( std::pair< unsigned int, unsigned int >( 1, 1 ) );
         }
      }
      // output ports
      for ( iterStreams = outLinkToModel.begin(); iterStreams != outLinkToModel.end(); ++iterStreams )
      {
         if ( iterStreams->second == iter->first )
         {
            VE_Model::Port* tempPort = tempModel->GetPort(-1);
            // outputs are from ports
            tempPort->SetPortNumber( streamPortIDS[ iterStreams->first ].first );
            tempPort->SetModelName( iterStreams->first );
            tempPort->SetDataFlowDirection( std::string( "output" ) );
            tempPort->GetPortLocation()->SetPoint( std::pair< unsigned int, unsigned int >( 1, 1 ) );
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
void BKPParser::StripCharacters( std::string& data, std::string character )
{
   for ( size_t index = 0; index < data.length(); )
   {
      index = data.find( character, index );
      if ( index != std::string::npos )
         data.erase( index, 1 );
   }
}
