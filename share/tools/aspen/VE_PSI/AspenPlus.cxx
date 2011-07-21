#include "stdafx.h"
#include "AspenPlus.h"
#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/Command.h>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <cmath>
#include <cctype>

#include "AspenPlusLUT.h"
#include "AspenIconData.h"

///////////////////////////////////////////////////////////////////////////////
AspenPlus::AspenPlus( std::string workingDir, std::string unitName )
    :
    aspendoc( new CASI::CASIDocument() ),
    redundantID( 0 )
{
    m_workingDir = workingDir;
    m_unitName = unitName;
}
///////////////////////////////////////////////////////////////////////////////
AspenPlus::~AspenPlus()
{
    delete aspendoc;
}
///////////////////////////////////////////////////////////////////////////////
void AspenPlus::OpenSimAndParse(const char * file)
{
    std::string fileName(file);
    std::string bkpExt(".bkp");
    std::string apwExt(".apw");
    ParseFile( ( m_workingDir + fileName + bkpExt ).c_str());
    CString filename = file;
    aspendoc->open( ( m_workingDir + fileName + apwExt ).c_str());
}
////////////////////////////////////////////////////////////////////////////////
void AspenPlus::OpenSim(const char * file)
{
    std::string fileName(file);
    std::string apwExt(".apw");
    CString filename = file;
    aspendoc->open( ( m_workingDir + fileName + apwExt ).c_str());
}
///////////////////////////////////////////////////////////////////////////////
void AspenPlus::closeFile()
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
    inLinkToModel.clear();
    outLinkToModel.clear();
    linkPoints.clear();
    linkTypes.clear();
    models.clear();
    iconLocations.clear();
    streamPortIDS.clear();
}
///////////////////////////////////////////////////////////////////////////////
void AspenPlus::saveFile()
{
    aspendoc->save();
}
///////////////////////////////////////////////////////////////////////////////
void AspenPlus::saveAs(const char * filename)
{
    aspendoc->saveAs(filename);
}
///////////////////////////////////////////////////////////////////////////////
void AspenPlus::showAspen(bool show)
{
    aspendoc->showAspen(show);
}
///////////////////////////////////////////////////////////////////////////////
void AspenPlus::step()
{
    aspendoc->step();
}
///////////////////////////////////////////////////////////////////////////////
void AspenPlus::ReinitAspen()
{
    aspendoc->initializeSolver();
}
///////////////////////////////////////////////////////////////////////////////
int AspenPlus::getNumComponents()
{
    return BlockInfoList.size(); //vectors are all same length
}
///////////////////////////////////////////////////////////////////////////////
std::string AspenPlus::getBlockType( const std::string& blockName,
                                    const std::string& flowsheetName )
{
    if(flowsheetName == "NULL")
    {
        return BlockInfoList["_main_sheet"][blockName].type;
    }
    else
    {
        return BlockInfoList[flowsheetName][blockName].type;
    }
}
///////////////////////////////////////////////////////////////////////////////
std::string AspenPlus::getBlockID( const std::string& blockName,
                                  const std::string& flowsheetName )
{
    if(flowsheetName == "NULL")
    {
        return BlockInfoList["_main_sheet"][blockName].id;
    }
    else
    {
        return BlockInfoList[flowsheetName][blockName].id;
    }
}
///////////////////////////////////////////////////////////////////////////////
float AspenPlus::getXCoord( int num )
{
    return xCoords[num];
}
///////////////////////////////////////////////////////////////////////////////
float AspenPlus::getYCoord( int num )
{
    return yCoords[num];
}
///////////////////////////////////////////////////////////////////////////////
float AspenPlus::getStreamXCoord( int streamIndex, int coordIndex )
{
    return streamCoordList[streamIndex].value[coordIndex].first;
}
///////////////////////////////////////////////////////////////////////////////
float AspenPlus::getStreamYCoord( int streamIndex, int coordIndex )
{
    return streamCoordList[streamIndex].value[coordIndex].second;
}
///////////////////////////////////////////////////////////////////////////////
std::string AspenPlus::getStreamId( int streamIndex )
{
    return streamCoordList[streamIndex].streamId;
}
///////////////////////////////////////////////////////////////////////////////
int AspenPlus::getStreamType( int streamIndex )
{
    return streamCoordList[streamIndex].streamType;
}
///////////////////////////////////////////////////////////////////////////////
int AspenPlus::getNumStream()
{
    return streamCoordList.size();
}
///////////////////////////////////////////////////////////////////////////////
int AspenPlus::getStreamSize( int index )
{
    return streamCoordList[index].value.size();
}
///////////////////////////////////////////////////////////////////////////////
void AspenPlus::ParseFile( const char * bkpFile )
{
    //Open file streams    
    std::ifstream inFile(bkpFile, std::ios::binary);

    std::map< std::pair< std::string, std::string >,
        std::vector< double > >::iterator lutMapIter;
    std::vector< double > lutVector;
    lutVector.resize( 6 );

    std::map< std::pair< std::string, std::string >, 
        std::vector< double > > lutMap;
    lutMap = GetAspenPlusLUT();

    std::map< std::string, std::pair< unsigned int, unsigned int > > imageData;
    imageData = GetAspenIconData();

    //make sure it is a valid file
    if(!inFile.is_open())
    {
        return;
    }
    
    //
    // Begin parsing
    //

    std::string discard;
    std::string temp;
    while(    temp.compare(0 , 7, "NumLibs", 0, 7))
    {
        getline(inFile, temp);
        if( inFile.eof() )
            break;
    }

    ///Do something here...
    {
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
    }

    ///Do something here...
    {
        getline(inFile, temp);
        std::stringstream numCatToken(temp);
        numCatToken >> discard;//Dump "NumLibs"
        numCatToken >> discard;//Dump "="
        int numCats;
        numCatToken >> numCats;
        numCats = numCats*2+2;
        
        int i = 0;
        
        while( i < numCats )
        {
            getline(inFile, temp);
            i++;
        }    
    }


    //
    // Block Info
    //

    //Get block id and type
    int count = 0;
    std::string compVer, compID, compName, compLib, compLibName;
    BlockInfo tempBlockInfo;
    std::string hierarchy;
    bool hierFlag = false;

    //get number of components/blocks
    getline(inFile, temp);
    int numComponents = atoi( temp.c_str() );

    while(count < numComponents)
    {
        getline(inFile, compVer);
        getline(inFile, compID);
        getline(inFile, compLibName);
        getline(inFile, compLib);
        getline(inFile, compName);

        std::string type;

        //remove newline
        std::stringstream tokenizer(compLibName);
        tokenizer >> type;
        std::transform(type.begin(), type.end(), type.begin(), std::tolower);
        tempBlockInfo.type = type;

        if(compLibName.find("Hierarchy") == std::string::npos)
            tempBlockInfo.hierarchical = false;
        else
            tempBlockInfo.hierarchical = true;

        //default hidden value for aspen is false
        tempBlockInfo.iconHidden = 0;

        if(compID.find(".") == std::string::npos)
        {
            std::stringstream tokenizer(compID);
            tokenizer >> tempBlockInfo.id;  //remove newline
            BlockInfoList["_main_sheet"][tempBlockInfo.id] = tempBlockInfo;
        }
        else
        {
            //parse out the name of the hierarchy block
            std::stringstream tokenizer(compID);
            tokenizer >> compID;  //remove newline
            size_t  pos = compID.find_last_of(".");
            std::string temp = compID.substr(0, pos);
            tempBlockInfo.id = compID.substr(pos+1, compID.size() - pos );
            BlockInfoList[temp][tempBlockInfo.id] = tempBlockInfo;
        }
        count++;
    }

    ////////////////////////////////////////////////////////////////////////////
    {
        //The following block contains the network data
        std::streampos beforeNetwork;
        beforeNetwork = inFile.tellg();

        //find the graphics section
        while(temp.compare(0, 16, " GRAPHICS_BACKUP", 0, 16)!= 0 && !inFile.eof())
        {
            getline(inFile, temp);
        }

        //Now we have passed the network data so record it
        std::streampos afterNetwork;
        afterNetwork = inFile.tellg();
        //go back to the beginning of the network
        inFile.seekg( beforeNetwork );
        // allocate memory:
        char* buffer = new char [afterNetwork - beforeNetwork];
        // read data as a block:
        inFile.read( buffer, (afterNetwork - beforeNetwork) );
        //std::ofstream tester4 ("tester4.txt");
        //tester4<<buffer<<std::endl;
        //tester4.close();
        std::string networkData( buffer );
        delete [] buffer;
        
        //build network information
        CreateNetworkInformation( networkData );
    }
    ////////////////////////////////////////////////////////////////////////////

    ////////////////////////////////////////////////////////////////////////////
    //Loop for Hierarchy
    std::map< std::string,
        std::map< std::string, BlockInfo > >::iterator sheetIter;
    try
    {
    for(sheetIter = BlockInfoList.begin();
        sheetIter != BlockInfoList.end();
        ++sheetIter)
    {
        //seek back to beginning of file because BlockInfoList is a map and
        //is alphabetical not in the file order.
        inFile.seekg(0);

        //construct PFS Entry
        std::string dataHeader;
        if( sheetIter->first.compare(0,17,"_main_sheet", 0, 17) == 0 )
        {
            dataHeader = "PFSVData";
        }
        else
        {
            dataHeader = sheetIter->first + " PFSVData"; 
        }

        //locate the PFSVData Entry
        while( temp.compare( 0, dataHeader.size(), dataHeader, 0, 
            dataHeader.size() )!= 0 && !inFile.eof())
        {
            getline(inFile, temp);
        }

        //throw out
        getline(inFile, temp);  //#PFS Objects
        getline(inFile, temp);  //Size
        getline(inFile, temp);  //Block

        //Read graphic blocks
        count =0;
        std::string id, version, icon, flag, section, at, labelAt, scaleMod;
        std::string annotation;
        std::map< std::string, std::map< std::string, BlockInfo > >::iterator blockIter = sheetIter; //BlockInfoList.find( sheetIter->first );
        std::map< std::string, BlockInfo >::iterator blockInfoIter;
        
        while(count < (int)blockIter->second.size())
        {
            std::getline(inFile, id);
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

            getline(inFile, temp); //dump next block header Should BE CHANGED
            //- when next entry is important

            //parse id to use for searching        
            std::stringstream idTokenizer(id);
            idTokenizer >> discard;
            std::string tempBlockId; 
            idTokenizer >> tempBlockId;

            BlockInfo tempBlockInfo_2;
            //sort the block id/type vector
            int entryIncr = 0;
            bool entryFound = false;
            std::string tempIcon;
            iconTokenizer >> tempIcon;
            tempIcon = tempIcon.substr(1, tempIcon.size() - 2);
            std::transform(tempIcon.begin(), tempIcon.end(), tempIcon.begin(), std::tolower);

            //replace - with _
            size_t found = tempIcon.find("-");
            while( found != std::string::npos )
            {
                tempIcon.replace(found, 1, "_");
                found = tempIcon.find("-");
            }
            
            blockInfoIter = blockIter->second.find( tempBlockId );
            blockInfoIter->second.icon = tempIcon;
            blockInfoIter->second.scale = scale;

            //find offset
            float left=0, right=0, bottom=0, top=0; //coords
            float widthOffset = 0;
            float heightOffset = 0;

            std::pair< std::string, std::string >
                blockKey(blockInfoIter->second.type,
                blockInfoIter->second.icon );
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
                //defaults to the values for the "BLOCK" icon entries
                left = -0.45;
                right = 0.45;
                top = 0.45;
                bottom = -0.45;
            }

            float iconWidth = right - left;
            float iconHeight = top - bottom;

            if(modifier == 0)
            {
                blockInfoIter->second.rotation = 0.0f;
                blockInfoIter->second.mirror = 0;
                widthOffset = abs(left/iconWidth);
                heightOffset = abs(top/iconHeight);
            }
            else if(modifier == 1)
            {
                blockInfoIter->second.rotation = 0.0f;
                blockInfoIter->second.mirror = 1;
                widthOffset = abs(right/iconWidth);
                heightOffset = abs(top/iconHeight);
            }
            else if(modifier == 2)
            {
                blockInfoIter->second.rotation = 0.0f;
                blockInfoIter->second.mirror = 2;
                widthOffset = abs(left/iconWidth);
                heightOffset = abs(bottom/iconHeight);
            }
            else if(modifier == 3)
            {
                blockInfoIter->second.rotation = 90.0f;
                blockInfoIter->second.mirror = 0;
                widthOffset = abs(top/iconWidth);
                heightOffset = abs(right/iconHeight);
            }
            else if(modifier == 4)
            {
                blockInfoIter->second.rotation = 270.0f;
                blockInfoIter->second.mirror = 0;
                widthOffset = abs(bottom/iconWidth);
                heightOffset = abs(left/iconHeight);
            }
            else if(modifier == 5)
            {
                blockInfoIter->second.rotation = 180.0f;
                blockInfoIter->second.mirror = 0;
                widthOffset = abs(right/iconWidth);
                heightOffset = abs(bottom/iconHeight);
            }
            else if(modifier == 6)
            {
                blockInfoIter->second.rotation = 270.0f;
                blockInfoIter->second.mirror = 2;
                widthOffset = abs(top/iconWidth);
                heightOffset = abs(left/iconHeight);
            }
            else if(modifier == 7)
            {
                blockInfoIter->second.rotation = 270.0f;
                blockInfoIter->second.mirror = 1;
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
            
            float width =
                imageData[ blockInfoIter->second.type+
                "_"+blockInfoIter->second.type+
                "_"+blockInfoIter->second.icon+
                ".xpm"].first;
            float height = 
                imageData[ blockInfoIter->second.type+
                "_"+blockInfoIter->second.type+
                "_"+blockInfoIter->second.icon+
                ".xpm"].second;

            blockInfoIter->second.width = width;
            blockInfoIter->second.height = height;

            iconLocations[sheetIter->first][ tempBlockId ] =
                std::pair< float, float >( scaledXCoords -
                ( width * widthOffset * 
                blockInfoIter->second.scale),
                scaledYCoords - ( height * heightOffset * 
                blockInfoIter->second.scale) );
            count++;
        }

        //locate minimum X - used for normalization
        float minX = 10000;
        float minY = 10000;
        std::map< std::string, std::pair< float, float > >::iterator iter;
        std::map< std::string, std::map< std::string, std::pair< float, float > > >::iterator iconLocationsIter = iconLocations.find( sheetIter->first );

        for (iter = iconLocationsIter->second.begin();
            iter != iconLocationsIter->second.end();
            iter++)
        {
            float currentX =
               iconLocationsIter->second[ iter->first ].first;
            float currentY =
                iconLocationsIter->second[ iter->first ].second;

            if(currentX < minX)
                minX = currentX;

            if(currentY < minY)
                minY = currentY;
        }

        //
        //Stream Info
        //

        //Gather stream information
        std::string streamId, streamVersion, streamFlag, streamType;
        std::string coordinates, tempR, tempR2;
        bool newStream = true, routeOne = false;;
        std::pair< float, float > tempCoords;
        int routeCount = 0;

        //contiously read all stream info to the legend or viewport entry
        while( temp.compare( 0, 8, "VIEWPORT", 0, 8 )!= 0 &&
            temp.compare( 0, 6, "LEGEND", 0, 6 )!= 0 &&
            !inFile.eof( ) )
        {
            if( temp.compare(0, 6, "STREAM", 0, 6)== 0 )//find "STREAM" entry
            {
                getline( inFile, streamId );
                getline( inFile, streamVersion );
                getline( inFile, streamFlag );
                getline( inFile, streamType );
                //Look for Stream type - needed for version 13.2
                while( streamType.compare( 0, 4, "TYPE", 0 , 4) != 0 )
                    getline( inFile, streamType );
                getline(inFile, temp);
                routeCount = 0;
                routeOne=false;
                newStream = true;
                
                //Look for Routes
                while( temp.compare( 0, 6, "STREAM", 0, 6 )!= 0 &&
                    temp.compare( 0, 8, "VIEWPORT", 0, 8 )!= 0 &&
                    temp.compare( 0, 6, "LEGEND", 0, 6 )!= 0 &&
                    routeCount < 3 &&
                    streamId.find( "#" ) == std::string::npos )
                {
                    //look for ROUTE heading
                    if( temp.compare( 0, 5, "ROUTE", 0, 5 ) == 0 )
                    {
                        getline( inFile, temp );
                        routeCount++;
                    }
                    //grab data
                    else if( temp.compare( 0, 2, "r ", 0, 2) == 0 )
                    {
                        while( temp.compare( 0, 5, "ROUTE", 0, 5) != 0 &&
                            temp.compare( 0, 2, "$ ", 0, 2)!= 0 &&
                            temp.compare( 0, 2, "At", 0, 2)!= 0 &&
                            temp.compare( 0, 5, "Label" ) != 0)
                        {
                            //seems you only need first 2 routes
                            if( routeCount == 1 || routeCount == 2 )
                            {
                                std::stringstream streamTokenizer( temp );
                                streamTokenizer >> tempR;
                                streamTokenizer >> tempR2;
                                streamTokenizer >> tempCoords.first;
                                streamTokenizer >> tempCoords.second;

                                if( routeCount == 1 )
                                {
                                    xy.value.push_back( tempCoords );
                                    routeOne = true;
                                }
                                if( routeCount == 2 )
                                {
                                    tempXY.value.push_back( tempCoords );
                                }
                            }

                            getline( inFile, temp );
                        }
                    }
                    else 
                    {
                        getline( inFile, temp );
                    }
                }
                //put 2nd route entry data ahead of first in vector
                int tempCount = 0;
                while( tempCount < (int)tempXY.value.size( ) )
                {
                    xy.value.insert( xy.value.begin(),
                        tempXY.value[tempCount] );
                    tempCount++;
                }
                
                std::stringstream idTokenizer( streamId );
                idTokenizer >> discard;
                idTokenizer >> xy.streamId;
                
                std::stringstream typeTokenizer( streamType );
                typeTokenizer >> discard;
                typeTokenizer >> xy.streamType;

                //add one streams values to vector 
                streamCoordList.push_back( xy );
                
                linkTypes[sheetIter->first][xy.streamId] = ( xy.streamType );
                //Create map of stream names to points
                for ( size_t k = 0; k < xy.value.size( ); ++k )
                {
                    //scaled up for icon spacing
                    float scaledX = xy.value.at( k ).first * 100;
                    //invert Y axis - flowsheets are inverted
                    float scaledY = -xy.value.at( k ).second * 100;

                    if(scaledX < minX)
                        minX = scaledX;

                    if(scaledY < minY)
                        minY = scaledY;

                    linkPoints[sheetIter->first][xy.streamId].push_back(
                        std::pair< float, float >( scaledX, scaledY ) );    
                }
                // add converted points for wx
                //empty temporary vector
                xy.value.erase(xy.value.begin(),xy.value.end() );
                //empty temporary vector
                tempXY.value.erase(tempXY.value.begin( ),tempXY.value.end( ) );
            }
            else 
            {
                getline( inFile, temp );
            }
        }

        //
        //NORMALIZE FOR WX
        //
        float normX = minX;
        float normY = minY;

        for(iter = iconLocationsIter->second.begin( );
            iter != iconLocationsIter->second.end( );
            iter++)
        {
            iconLocationsIter->second[ iter->first ].first =
                iconLocationsIter->second[iter->first].first - normX;
            iconLocationsIter->second[iter->first].second =
                iconLocationsIter->second[iter->first].second - normY;
        }

        std::map< std::string, std::vector< std::pair< float, float > > >::
            iterator iter2;
        for( iter2 = linkPoints[sheetIter->first].begin( );
            iter2 != linkPoints[sheetIter->first].end( );
            iter2++)
        {
            for( int element = 0;
                element <
                (int)linkPoints[sheetIter->first][ iter2->first ].size();
                element++)
            {
                linkPoints[sheetIter->first][ iter2->first ][element].first =
                    linkPoints[sheetIter->first][ iter2->first ][element].
                    first - normX;
                linkPoints[sheetIter->first][ iter2->first ][element].second =
                    linkPoints[sheetIter->first][ iter2->first ][element].
                    second - normY;
            }
        }
    }
    }
    catch(std::exception& e)
    {
        std::ofstream exOut("ex_out.txt");
        exOut << e.what() <<std::endl;
        exOut.close();
    }

    inFile.close();
}
///////////////////////////////////////////////////////////////////////////////
void AspenPlus::CreateNetworkInformation( std::string& networkData )
{
    // strip the new line characters
    StripCharacters( networkData, "\n" );
    // strip the <cr>
    StripCharacters( networkData, "\r" );

    //Obtain network chunk
    size_t networkBegin = networkData.find( std::string( "? SETUP MAIN ?" ) );
    size_t networkEnd = networkData.find( std::string( "GRAPHICS_BACKUP" ) );
    std::string network;
    network.append( networkData, networkBegin, (networkEnd - networkBegin) );

    size_t tagBegin = 0;
    size_t tagEnd = 0;

    // create the maps and network connectivity
    std::string blockName;
    std::string hierName = "_main_sheet";
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
        if ( tagBegin != std::string::npos && tagEnd != std::string::npos )
        {
            blockData.append( network, tagBegin, (tagEnd - tagBegin) );
            tagBegin = tagEnd + 1;
            if(blockData.find(std::string("BLOCK HIERARCHY")) !=
                std::string::npos )
            {
                StripCharacters( blockData, "?" );
                StripCharacters( blockData, "\"" );
                std::stringstream networkToks(blockData);
                while(networkToks >> hierName);
            }
            else if(blockData.find(std::string("BLOCK BLKID")) !=
                std::string::npos )
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
                    if ( ( vectorTokens.at( i ) == std::string( "=" ) ) &&
                        ( vectorTokens.at( i - 1 ) == std::string( "BLKID" ) ))
                    {
                        blockName = vectorTokens.at( ++i );
                        StripCharacters( blockName, "\"" );
                        models[hierName][blockName] = redundantID++;
                    }
                    // this are the input links/streams that connect to 
                    //this particular block
                    else if ( (vectorTokens.at( i ) == std::string ( "=" )) &&
                        (vectorTokens.at( i - 1 ) == std::string( "IN" )) )
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
                    // this are the output links/streams that connect to
                    //this particular block
                    else if ( (vectorTokens.at( i ) == std::string ( "=" )) &&
                        (vectorTokens.at( i - 1 ) == std::string( "OUT" )) )
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
            else if(blockData.find(std::string("CONNECT BLKID")) !=
                std::string::npos )
            {
                StripCharacters( blockData, "\\" );
                std::stringstream networkToks(blockData);
                std::string token;

                //discard "CONNECT" "BLKID" "=" & "name"
                for( size_t i = 0; i < 4; i++)
                {
                    networkToks >> token;
                }

                //populate vector with remaining tokens
                std::vector< std::string >  vectorTokens;
                while(networkToks >> token)
                vectorTokens.push_back( token );

                for ( size_t i = 0; i < vectorTokens.size(); ++i )
                {
                    // this are the input links/streams that connect to
                    //this particular block
                    if ( ( vectorTokens.at( i ) == std::string ( "=" ) ) &&
                        ( vectorTokens.at( i - 1 ) == std::string( "IN" ) ) )
                    {
                        if(vectorTokens.at( i+2 ).find("\"") == std::string::npos)
                        {
                            inLinkToModel[hierName][vectorTokens.at( i+=2 )] =
                                blockName;
                            ++i;
                        }
                    }
                    // this are the output links/streams that connect to
                    //this particular block
                    else if ( ( vectorTokens.at( i ) == std::string ( "=" ) )
                        && (vectorTokens.at( i - 1 ) == std::string( "OUT" )) )
                    {
                        if(vectorTokens.at( i+2 ).find("\"") ==
                            std::string::npos )
                        {
                            outLinkToModel[hierName][vectorTokens.at( i+=2 )]
                            = blockName;
                            ++i;
                        }
                    }
                }
            }
        }
    }
    while( tagBegin < network.size() - 1 );
}
///////////////////////////////////////////////////////////////////////////////
void AspenPlus::CreateNetworkLinks
    ( ves::open::xml::model::NetworkPtr subNetwork, const std::string& hierName )
{
    // remove duplicate points
    std::map< std::string, std::vector< std::pair< float, float > > >::iterator
    pointsIter;
    for ( pointsIter = linkPoints[hierName].begin();
        pointsIter != linkPoints[hierName].end();
        ++pointsIter )
    {
        std::vector< std::pair< float, float > > tempPoints;
        tempPoints = pointsIter->second;
        std::vector< std::pair< float, float > >::iterator pairIter;
        for ( pairIter = tempPoints.begin(); pairIter != tempPoints.end(); )
        {
            // need to remove duplicate points
            std::vector< std::pair< float, float > >::iterator tempPairIter;
            tempPairIter =
                std::find( pairIter+1, tempPoints.end(), *pairIter );
            if ( tempPairIter != tempPoints.end() )
            {
                tempPoints.erase( tempPairIter );
            }
            else
            {
                ++pairIter;
            }
        }
        pointsIter->second = tempPoints;
    }
    
    std::map< std::string, std::string >::iterator iter;
    // create links for the network
    int counter = 0;
    for ( iter = inLinkToModel[hierName].begin();
        iter != inLinkToModel[hierName].end();
        ++iter )
    {
        std::map< std::string, std::string >::iterator fromModel;
        fromModel = outLinkToModel[hierName].find( iter->first );

        //find matching port
        if ( fromModel != outLinkToModel[hierName].end() )
        {
            //define link
            // these are unique remember...
            std::string toPortName = iter->first;
            // these are unique remember...
            std::string fromPortName = fromModel->first;
            
            std::string toModelName;
            std::string fromModelName;
            if(hierName == "_main_sheet")
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
            streamPortIDS[hierName][ iter->first ] =
                std::pair< int, int >( toPortId, fromPortId );
            
            //Now we create a link
            ves::open::xml::model::LinkPtr
                xmlLink( new ves::open::xml::model::Link() );
            xmlLink->GetFromModule()->SetData(
                fromModelName, static_cast< long int >( fromModelId ) );
            xmlLink->GetToModule()->SetData(
                toModelName, static_cast< long int >( toModelId ) );
            
            xmlLink->SetLinkName(iter->first);
            xmlLink->SetLinkType(linkTypes[hierName][iter->first]);
            
            *(xmlLink->GetFromPort()) = static_cast< long int >( fromPortId );
            *(xmlLink->GetToPort()) = static_cast< long int >( toPortId );
            
            for ( size_t j = linkPoints[hierName][ fromPortName ].size();
                j > 0 ; --j )
            {
                // I am not sure why we need to reverse the points but we do
                std::pair< unsigned int, unsigned int > tempLinkPointUI;
                tempLinkPointUI = 
                    std::pair< unsigned int, unsigned int >( 
                    unsigned int( linkPoints[hierName][ fromPortName ].at( j - 1 ).first ), 
                    unsigned int( linkPoints[hierName][ fromPortName ].at( j - 1 ).second ) );
                xmlLink->GetLinkPoint(
                    linkPoints[hierName][ fromPortName ].size() - j )->
                    SetPoint( tempLinkPointUI );
            }
            subNetwork->AddLink( xmlLink );
        }
        
        //create stand alone input streams
        else
        {
            //create the dummy entry in the models list
            models[hierName][iter->first + "_dummy_connection"] =
                redundantID++;
            
            //define link
            // these are unique remember...
            std::string toPortName = iter->first;
            // these are unique remember...
            std::string fromPortName = iter->first + "_dummy_connection";
            
            std::string toModelName;
            std::string fromModelName;
            
            if(hierName == "_main_sheet")
            {
                toModelName = iter->second;
                fromModelName = iter->first + "_dummy_connection";
            }
            else
            {
                toModelName = hierName + "." + iter->second;
                fromModelName =
                    hierName + "." + iter->first + "_dummy_connection";
            }
            
            int toPortId = counter++;
            int fromPortId = counter++;
            int toModelId = models[hierName][ iter->second ];
            int fromModelId =
                models[hierName][ iter->first + "_dummy_connection" ];
            streamPortIDS[hierName][ iter->first ] =
                std::pair< int, int >( toPortId, fromPortId );
            
            //Now we create a link
            ves::open::xml::model::LinkPtr
                xmlLink( new ves::open::xml::model::Link() );
            xmlLink->GetFromModule()->SetData(
                fromModelName, static_cast< long int >( fromModelId ) );
            xmlLink->GetToModule()->SetData(
                toModelName, static_cast< long int >( toModelId ) );
            
            xmlLink->SetLinkName(iter->first);
            xmlLink->SetLinkType(linkTypes[hierName][iter->first]);
            
            *(xmlLink->GetFromPort()) = static_cast< long int >( fromPortId );
            *(xmlLink->GetToPort()) = static_cast< long int >( toPortId );
            
            for ( size_t j = linkPoints[hierName][ toPortName ].size();
                j > 0 ; --j )
            {
                // I am not sure why we need to reverse the points but we do
                std::pair< unsigned int, unsigned int > tempLinkPointUI;
                tempLinkPointUI = 
                    std::pair< unsigned int, unsigned int >( 
                    unsigned int( linkPoints[hierName][ toPortName ].at( j - 1 ).first ), 
                    unsigned int( linkPoints[hierName][ toPortName ].at( j - 1 ).second ) );
                xmlLink->GetLinkPoint(
                    linkPoints[hierName][ toPortName ].size() - j )->
                    SetPoint( tempLinkPointUI );
            }
         
            //add location for dummy icon
            iconLocations[hierName][iter->first+"_dummy_connection"] = 
                linkPoints[hierName][toPortName].at(
                linkPoints[hierName][ toPortName ].size()-1);
            
            //add information for dummy block
            BlockInfoList
                [hierName][iter->first+"_dummy_connection"].type =
                "standalone";
            BlockInfoList
                [hierName][iter->first+"_dummy_connection"].icon =
                "standalone";
            BlockInfoList
                [hierName][iter->first+"_dummy_connection"].scale = 1;
            BlockInfoList
                [hierName][iter->first+"_dummy_connection"].rotation = 0;
            BlockInfoList
                [hierName][iter->first+"_dummy_connection"].mirror = 0;
            BlockInfoList
                [hierName][iter->first+"_dummy_connection"].hierarchical =
                false;
            BlockInfoList
                [hierName][iter->first+"_dummy_connection"].iconHidden =
                1;
         
            //added for output port on dummy block
            outLinkToModel[hierName][iter->first] =
                iter->first+"_dummy_connection";

            //add the link
            subNetwork->AddLink( xmlLink );
        }
    }

    //create standalone output streams
    for ( iter = outLinkToModel[hierName].begin();
        iter != outLinkToModel[hierName].end();
        ++iter )
    {
        std::map< std::string, std::string >::iterator toModel;
        toModel = inLinkToModel[hierName].find( iter->first );

        //make sure the link is standalone
        if ( toModel == inLinkToModel[hierName].end() )
        {
            //create the dummy entry in the models list
            models[hierName][iter->first + "_dummy_connection"] =
                redundantID++;

            //define link
            // these are unique remember...
            std::string toPortName = iter->first + "_dummy_connection";
            // these are unique remember...
            std::string fromPortName = iter->first;

            std::string toModelName;
            std::string fromModelName;
            if(hierName == "_main_sheet")
            {
                toModelName = iter->first + "_dummy_connection";
                fromModelName = iter->second;
            }
            else
            {
                toModelName =
                    hierName + "." + iter->first + "_dummy_connection";
                fromModelName = hierName + "." + iter->second;
            }

            int toPortId = counter++;
            int fromPortId = counter++;
            int toModelId =
                models[hierName][ iter->first + "_dummy_connection"];
            int fromModelId = models[hierName][ iter->second ];
            streamPortIDS[hierName][ iter->first ] =
                std::pair< int, int >( toPortId, fromPortId );
         
            //Now we create a link
            ves::open::xml::model::LinkPtr xmlLink(
                new ves::open::xml::model::Link() );
            xmlLink->GetFromModule()->SetData(
                fromModelName, static_cast< long int >( fromModelId ) );
            xmlLink->GetToModule()->SetData(
                toModelName, static_cast< long int >( toModelId ) );
         
            xmlLink->SetLinkName(iter->first);
            xmlLink->SetLinkType(linkTypes[hierName][iter->first]);
         
            *(xmlLink->GetFromPort()) = static_cast< long int >( fromPortId );
            *(xmlLink->GetToPort()) = static_cast< long int >( toPortId );

            for ( size_t j = linkPoints[hierName][ fromPortName ].size();
                j > 0 ; --j )
            {
                // I am not sure why we need to reverse the points but we do
                std::pair< unsigned int, unsigned int > tempLinkPointUI;
                tempLinkPointUI = 
                    std::pair< unsigned int, unsigned int >( 
                    unsigned int( linkPoints[hierName][ fromPortName ].at( j - 1 ).first ), 
                    unsigned int( linkPoints[hierName][ fromPortName ].at( j - 1 ).second ) );
                xmlLink->GetLinkPoint(
                    linkPoints[hierName][ fromPortName ].size() - j )->
                    SetPoint( tempLinkPointUI );
            }

            //add location for dummy icon
            iconLocations[hierName][iter->first+"_dummy_connection"] =
                linkPoints[hierName][fromPortName].at(0);
          
            //add information for dummy block
            BlockInfoList[hierName][iter->first+"_dummy_connection"].type =
                "standalone";
            BlockInfoList
                [hierName][iter->first+"_dummy_connection"].icon =
                "standalone";
            BlockInfoList
                [hierName][iter->first+"_dummy_connection"].scale = 1;
            BlockInfoList
                [hierName][iter->first+"_dummy_connection"].rotation = 0;
            BlockInfoList
                [hierName][iter->first+"_dummy_connection"].mirror = 0;
            BlockInfoList
                [hierName][iter->first+"_dummy_connection"].hierarchical =
                false;
            BlockInfoList
                [hierName][iter->first+"_dummy_connection"].iconHidden =
                1;
         
            //added for output port on dummy block         
            inLinkToModel[hierName][iter->first] =
                iter->first+"_dummy_connection";

            //add the link         
            subNetwork->AddLink( xmlLink );
        }
    }
}
///////////////////////////////////////////////////////////////////////////////
std::string AspenPlus::CreateNetwork( void )
{
    // then create the appropriate models
    // then put them all together and for a network string
    // Here we wshould loop over all of the following
    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    ves::open::xml::model::NetworkPtr
        mainNetwork( new ves::open::xml::model::Network() );
    ves::open::xml::model::SystemPtr
        veSystem( new ves::open::xml::model::System() );

    // create default state info section
    ves::open::xml::DataValuePairPtr dvpPtr;
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "m_xUserScale", 1.0 );
    mainNetwork->AddDataValuePair( dvpPtr );
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "m_yUserScale", 1.0 );
    mainNetwork->AddDataValuePair( dvpPtr );
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "nPixX", static_cast< long int >( 20 ) );
    mainNetwork->AddDataValuePair( dvpPtr );
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "nPixY", static_cast< long int >( 20 ) );
    mainNetwork->AddDataValuePair( dvpPtr );
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "nUnitX", static_cast< long int >( 200 ) );
    mainNetwork->AddDataValuePair( dvpPtr );
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "nUnitY", static_cast< long int >( 200 ) );
    mainNetwork->AddDataValuePair( dvpPtr );
    veSystem->AddNetwork( mainNetwork );

    CreateNetworkLinks(mainNetwork, "_main_sheet");

    // Loop over the top networks blocks
    std::map< std::string, int >::iterator blockIter;
    for ( blockIter = models["_main_sheet"].begin();
        blockIter != models["_main_sheet"].end();
        ++blockIter )
    {
        ves::open::xml::model::ModelPtr
            tempModel( new ves::open::xml::model::Model() );
        tempModel->SetModelID( blockIter->second );
        tempModel->SetPluginName( blockIter->first );
        tempModel->SetPluginType( "APUOPlugin" );
        tempModel->SetVendorName( m_unitName );
        tempModel->
            SetIconFilename(BlockInfoList["_main_sheet"][blockIter->first].type
            +"_"+BlockInfoList["_main_sheet"][blockIter->first].type+"_"
            +BlockInfoList["_main_sheet"][blockIter->first].icon + ".xpm");
        tempModel->
            SetIconRotation(BlockInfoList["_main_sheet"][blockIter->first].
            rotation);
        tempModel->
            SetIconScale(BlockInfoList["_main_sheet"][blockIter->first].
            scale);
        tempModel->
            SetIconMirror(BlockInfoList["_main_sheet"][blockIter->first].
            mirror);
        tempModel->
            GetIconLocation()->SetPoint( std::pair< unsigned int, unsigned int >(
            iconLocations["_main_sheet"][ blockIter->first ].first,
            iconLocations["_main_sheet"][ blockIter->first ].second ) );
        tempModel->
            SetIconHiddenFlag( BlockInfoList["_main_sheet"][blockIter->first].
            iconHidden );

        double iOriginX = iconLocations["_main_sheet"][ blockIter->first ].first;
        double iOriginY = iconLocations["_main_sheet"][ blockIter->first ].second;
        double iWidth = BlockInfoList["_main_sheet"][blockIter->first].width *
            BlockInfoList["_main_sheet"][blockIter->first].scale;
        double iHeight = BlockInfoList["_main_sheet"][blockIter->first].height *
            BlockInfoList["_main_sheet"][blockIter->first].scale;

        // input ports
        std::map< std::string, std::string >::iterator streamIter;
        for ( streamIter = inLinkToModel["_main_sheet"].begin();
            streamIter != inLinkToModel["_main_sheet"].end();
            ++streamIter )
        {
            if ( streamIter->second == blockIter->first )
            {
                ves::open::xml::model::PortPtr tempPort =
                    tempModel->GetPort(-1);
                // inputs are to ports
                tempPort->
                    SetPortNumber( streamPortIDS["_main_sheet"][ streamIter->first ].first );
                tempPort->SetDataFlowDirection( std::string( "input" ) );

                //PATCH
                //This code is necessary because some of the captured icons are
                //NOT properly sized
                //this will move any ports outside the icon into the icon
                double portX = 
                    linkPoints["_main_sheet"][ streamIter->first ][0].
                    first - iOriginX;
                double portY = 
                    linkPoints["_main_sheet"][ streamIter->first ][0].
                    second - iOriginY;

                if( portX < 0 )
                {
                    portX = 0;
                }
                if( portY < 0 )
                {
                    portY = 0;
                }

                if( portX > iWidth)
                {
                    portX = iWidth;
                }
                if( portY > iHeight )
                {
                    portY = iHeight;
                }
                
                tempPort->
                    GetPortLocation()->SetPoint( std::pair< unsigned int, unsigned int >
                    ( portX, portY ) );
            }
        }
        // output ports
        for ( streamIter = outLinkToModel["_main_sheet"].begin();
            streamIter != outLinkToModel["_main_sheet"].end();
            ++streamIter )
        {
            if ( streamIter->second == blockIter->first )
            {
                ves::open::xml::model::PortPtr tempPort =
                    tempModel->GetPort(-1);
                // outputs are from ports
                tempPort->
                    SetPortNumber( streamPortIDS["_main_sheet"][ streamIter->first ].second );
                tempPort->SetDataFlowDirection( std::string( "output" ) );

                //PATCH
                //This code is necessary because some of the captured icons are
                //NOT properly sized
                //this will move any ports outside the icon into the icon
                double portX = linkPoints["_main_sheet"][ 
                    streamIter->first ][linkPoints["_main_sheet"][ 
                    streamIter->first ].size()-1].first - iOriginX;
                double portY = linkPoints["_main_sheet"][ 
                    streamIter->first ][linkPoints["_main_sheet"][ 
                    streamIter->first].size()-1].second - iOriginY;

                if( portX < 0 )
                {
                    portX = 0;
                }
                if( portY < 0 )
                {
                    portY = 0;
                }

                if( portX > iWidth)
                {
                    portX = iWidth;
                }
                if( portY > iHeight )
                {
                    portY = iHeight;
                }
                
                tempPort->
                    GetPortLocation()->SetPoint( std::pair< unsigned int, unsigned int >
                    ( portX, portY ) );
            }
        }

        //recursively parse subsystems of each block
        if( tempModel->GetIconFilename().find("hierarchy") !=
            std::string::npos )
        ParseSubSystem( tempModel, blockIter->first );

        //attach model to top system
        veSystem->AddModel(tempModel);
    }

    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >
        ( veSystem, "veSystem" ) );

    std::string fileName( "returnString" );
    ves::open::xml::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();
    netowrkWriter.WriteXMLDocument( nodes, fileName, "Network" );
    return fileName;
}
///////////////////////////////////////////////////////////////////////////////
void AspenPlus::ParseSubSystem( ves::open::xml::model::ModelPtr model,
                               const std::string& networkName)
{
    ves::open::xml::model::SystemPtr
        subSystem( new ves::open::xml::model::System() );
    ves::open::xml::model::NetworkPtr
        subNetwork( new ves::open::xml::model::Network() );
    subSystem->AddNetwork(subNetwork);

    CreateNetworkLinks(subNetwork, networkName);

    // Loop over the top networks blocks
    std::map< std::string, int >::iterator blockIter;
    for ( blockIter = models[networkName].begin(); 
        blockIter != models[networkName].end();
        ++blockIter )
    {
        ves::open::xml::model::ModelPtr
            tempModel( new ves::open::xml::model::Model() );
        tempModel->SetModelID( blockIter->second );
        tempModel->SetPluginName( blockIter->first );
        tempModel->SetPluginType( "APUOPlugin" );
        tempModel->SetVendorName( m_unitName );
        tempModel->
            SetIconFilename(BlockInfoList[networkName][blockIter->first].type +
            "_" + BlockInfoList[networkName][blockIter->first].type+"_" +
            BlockInfoList[networkName][blockIter->first].icon + ".xpm" );
        tempModel->
            SetIconRotation(
            BlockInfoList[networkName][blockIter->first].rotation );
        tempModel->
            SetIconScale(
            BlockInfoList[networkName][blockIter->first].scale );
        tempModel->
            SetIconMirror(
            BlockInfoList[networkName][blockIter->first].mirror );
        tempModel->
            GetIconLocation()->SetPoint(
            std::pair< unsigned int, unsigned int >(
            iconLocations[networkName][blockIter->first].first,
            iconLocations[networkName][blockIter->first].second ) );
        tempModel->
            SetIconHiddenFlag( BlockInfoList[networkName][blockIter->first].
            iconHidden );

        double iOriginX = iconLocations[networkName][ blockIter->first ].first;
        double iOriginY = iconLocations[networkName][ blockIter->first ].second;
        double iWidth = BlockInfoList[networkName][blockIter->first].width *
            BlockInfoList[networkName][blockIter->first].scale;
        double iHeight = BlockInfoList[networkName][blockIter->first].height *
            BlockInfoList[networkName][blockIter->first].scale;

        // input ports
        std::map< std::string, std::string >::iterator streamIter;
        for ( streamIter = inLinkToModel[networkName].begin( );
            streamIter != inLinkToModel[networkName].end( );
            ++streamIter )
        {
            if ( streamIter->second == blockIter->first )
            {
                ves::open::xml::model::PortPtr tempPort =
                    tempModel->GetPort(-1);

                // inputs are to ports
                tempPort->
                    SetPortNumber( streamPortIDS[networkName][ streamIter->first ].first );
                tempPort->SetDataFlowDirection( std::string( "input" ) );
                
                //PATCH
                //This code is necessary because some of the captured icons are
                //NOT properly sized
                //this will move any ports outside the icon into the icon
                double portX = 
                    linkPoints[networkName][ streamIter->first ][0].
                    first - iOriginX;
                double portY = 
                    linkPoints[networkName][ streamIter->first ][0].
                    second - iOriginY;

                if( portX < 0 )
                {
                    portX = 0;
                }
                if( portY < 0 )
                {
                    portY = 0;
                }

                if( portX > iWidth)
                {
                    portX = iWidth;
                }
                if( portY > iHeight )
                {
                    portY = iHeight;
                }
                
                tempPort->
                    GetPortLocation()->SetPoint( std::pair< unsigned int, unsigned int >
                    ( portX, portY ) );
            }
        }
        // output ports
        for ( streamIter = outLinkToModel[networkName].begin( );
            streamIter != outLinkToModel[networkName].end( );
            ++streamIter )
        {
            if ( streamIter->second == blockIter->first )
            {
                ves::open::xml::model::PortPtr tempPort =
                    tempModel->GetPort(-1);
                // outputs are from ports
                tempPort->
                    SetPortNumber( streamPortIDS[networkName][ streamIter->first ].second );
                tempPort->SetDataFlowDirection( std::string( "output" ) );
                
                //PATCH
                //This code is necessary because some of the captured icons are
                //NOT properly sized
                //this will move any ports outside the icon into the icon
                double portX = linkPoints[networkName][ 
                    streamIter->first][linkPoints[networkName][ 
                    streamIter->first].size()-1].first - iOriginX;
                double portY = linkPoints[networkName][ 
                    streamIter->first ][linkPoints[networkName][ 
                    streamIter->first ].size()-1].second - iOriginY;

                if( portX < 0 )
                {
                    portX = 0;
                }
                if( portY < 0 )
                {
                    portY = 0;
                }

                if( portX > iWidth)
                {
                    portX = iWidth;
                }
                if( portY > iHeight )
                {
                    portY = iHeight;
                }
                
                tempPort->
                    GetPortLocation()->SetPoint( std::pair< unsigned int, unsigned int >
                    ( portX, portY ) );
            }
        }

        //recursively parse subsystems of each block
        if( tempModel->GetIconFilename().find("hierarchy") !=
            std::string::npos )
        {
            ParseSubSystem( tempModel, networkName + "." + blockIter->first );
        }

        //attach model to top system
        subSystem->AddModel(tempModel);
    }
    model->SetSubSystem(subSystem);
}
///////////////////////////////////////////////////////////////////////////////
void AspenPlus::StripCharacters( std::string& data, const std::string& character )
{
    for ( size_t index = 0; index < data.length(); )
    {
        index = data.find( character, index );
        if ( index != std::string::npos )
        {
            data.erase( index, 1 );
        }
    }
}
///////////////////////////////////////////////////////////////////////////////
//BLOCKS
void AspenPlus::ReinitBlock( const std::string& modname)
{
    aspendoc->reinitializeBlock( modname.c_str() );
}
///////////////////////////////////////////////////////////////////////////////
//BLOCKS
std::string AspenPlus::GetInputModuleParams( const std::string& modname)
{
    CASI::CASIObj cur_block =
        aspendoc->getBlockByName( CString( modname.c_str( ) ) );
    cur_block.processBlockInputs();
    
    ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
    std::vector<std::string> paramList;
    //input variables;
    params->SetCommandName((modname+"InputParams").c_str());

    int numOfVars = cur_block.getNumberOfInputVars();
    for(int i = 0; i < numOfVars; i++)
    {
        paramList.push_back( (char*)LPCTSTR(cur_block.getInputVarName(i)) );
    }

    ves::open::xml::DataValuePairPtr
        inpParams( new ves::open::xml::DataValuePair() );
    inpParams->SetData("params",paramList);
    params->AddDataValuePair( inpParams );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, 
    std::string >( params, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}
///////////////////////////////////////////////////////////////////////////////
std::string AspenPlus::GetInputModuleParamProperties( const std::string& modname,
                                                     const std::string& paramName)
{
    CASI::CASIObj cur_block =
        aspendoc->getBlockByName(CString(modname.c_str()));
    cur_block.processBlockInputs();

    unsigned int j;

    ves::open::xml::CommandPtr properties( new ves::open::xml::Command() );
    const int propSize = 24;
    properties->SetCommandName((modname+paramName).c_str());

    ves::open::xml::DataValuePairPtr Props[propSize];
    for (j=0; j<propSize; j++)
    {
        Props[j] =
            ves::open::xml::DataValuePairPtr(
            new ves::open::xml::DataValuePair() );
        Props[j]->SetDataType("STRING");
        properties->AddDataValuePair( Props[j] );
    }
    CASI::Variable tempvar =
        cur_block.getInputVarByName(CString(paramName.c_str()));
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

    Props[j]->SetDataName("Dimension");
    std::stringstream convert;
    convert << tempvar.getDimension();
    Props[j++]->SetDataString(convert.str().c_str());

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

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;

    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >
        ( properties, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}
///////////////////////////////////////////////////////////////////////////////
std::string AspenPlus::GetOutputModuleParams( const std::string& modname)
{
    CASI::CASIObj cur_block =
        aspendoc->getBlockByName(CString(modname.c_str()));
    cur_block.processBlockOutputs();
    
    ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
    std::vector<std::string> paramList;
    //input variables;
    params->SetCommandName((modname+"OutputParams").c_str());

    int numOfVars = cur_block.getNumberOfOutputVars();
    for(int i = 0; i < numOfVars; i++)
    {
        paramList.push_back( (char*)LPCTSTR(cur_block.getOutputVarName(i)) );
    }

    ves::open::xml::DataValuePairPtr
        inpParams( new ves::open::xml::DataValuePair() );
    inpParams->SetData("params",paramList);
    params->AddDataValuePair( inpParams );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( 
    std::pair< ves::open::xml::XMLObjectPtr, std::string >
    ( params, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}
///////////////////////////////////////////////////////////////////////////////
std::string AspenPlus::GetOutputModuleParamProperties(const std::string& modname,
                                                      const std::string& paramName)
{
    CASI::CASIObj cur_block =
        aspendoc->getBlockByName(CString(modname.c_str()));
    cur_block.processBlockOutputs();

    unsigned int j;

    ves::open::xml::CommandPtr properties( new ves::open::xml::Command() );
    const int propSize=23;
    properties->SetCommandName((modname+paramName).c_str());

    ves::open::xml::DataValuePairPtr Props[propSize];
    for (j=0; j<propSize; j++)
    {
        Props[j] =
            ves::open::xml::DataValuePairPtr(
            new ves::open::xml::DataValuePair() );
        Props[j]->SetDataType("STRING");
        properties->AddDataValuePair( Props[j] );
    }
    CASI::Variable tempvar =
        cur_block.getOutputVarByName(CString(paramName.c_str()));
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

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;

    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >
        ( properties, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}
///////////////////////////////////////////////////////////////////////////////
//Streams
std::string AspenPlus::GetStreamInputModuleParams( const std::string& modname)
{
    CASI::CASIObj cur_stream =
        aspendoc->getStreamByName(CString(modname.c_str()));

    ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
    std::vector<std::string> paramList;
    //input variables;
    params->SetCommandName((modname+"InputParams").c_str());
    
    int numOfVars = cur_stream.getNumberOfInputVars();
    for(int i = 0; i < numOfVars; i++)
    {
        paramList.push_back((char*)LPCTSTR(cur_stream.getInputVarName(i)));
    }

    ves::open::xml::DataValuePairPtr
        inpParams( new ves::open::xml::DataValuePair() );
    inpParams->SetData("params",paramList);
    params->AddDataValuePair( inpParams );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( 
    std::pair< ves::open::xml::XMLObjectPtr, std::string >
    ( params, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}
///////////////////////////////////////////////////////////////////////////////
std::string AspenPlus::GetStreamInputModuleParamProperties(
    const std::string& modname, const std::string& paramName )
{
    CASI::CASIObj cur_stream =
        aspendoc->getStreamByName( CString(modname.c_str( ) ) );
    unsigned int j;

    ves::open::xml::CommandPtr properties( new ves::open::xml::Command() );
    const int propSize=23;
    properties->SetCommandName((modname+paramName).c_str());

    ves::open::xml::DataValuePairPtr Props[propSize];
    for (j=0; j<propSize; j++)
    {
        Props[j] =
            ves::open::xml::DataValuePairPtr(
            new ves::open::xml::DataValuePair() );
        Props[j]->SetDataType("STRING");
        properties->AddDataValuePair( Props[j] );
    }
    CASI::Variable tempvar =
        cur_stream.getInputVarByName( paramName.c_str( ) );
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

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;

    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >
        ( properties, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}
///////////////////////////////////////////////////////////////////////////////
std::string AspenPlus::GetStreamOutputModuleParams( const std::string& modname)
{
    CASI::CASIObj cur_stream =
        aspendoc->getStreamByName( CString( modname.c_str( ) ) );

    ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
    std::vector<std::string> paramList;
    //input variables;
    params->SetCommandName((modname+"OutputParams").c_str());

    int numOfVars = cur_stream.getNumberOfOutputVars();
    for(int i = 0; i < numOfVars; i++)
    {
        paramList.push_back((char*)LPCTSTR(cur_stream.getOutputVarName(i)));
    }

    ves::open::xml::DataValuePairPtr
        inpParams( new ves::open::xml::DataValuePair( ) );
    inpParams->SetData("params",paramList);
    params->AddDataValuePair( inpParams );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >
    ( params, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}
///////////////////////////////////////////////////////////////////////////////
std::string AspenPlus::GetStreamOutputModuleParamProperties(
    const std::string& modname, const std::string& paramName )
{
    CASI::CASIObj cur_stream =
        aspendoc->getStreamByName( CString(modname.c_str( ) ) );
    unsigned int j;

    ves::open::xml::CommandPtr
        properties( new ves::open::xml::Command() );
    const int propSize=23;
    properties->SetCommandName( ( modname+paramName ).c_str( ) );

    ves::open::xml::DataValuePairPtr Props[propSize];
    for (j = 0; j < propSize; j++)
    {
        Props[j] =
            ves::open::xml::DataValuePairPtr(
            new ves::open::xml::DataValuePair( ) );
        Props[j]->SetDataType("STRING");
        properties->AddDataValuePair( Props[j] );
    }
    CASI::Variable tempvar =
        cur_stream.getOutputVarByName( CString( paramName.c_str( ) ) );
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

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;

    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >
        ( properties, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}
///////////////////////////////////////////////////////////////////////////////
void AspenPlus::SetWorkingDir( const std::string& dir )
{
    m_workingDir = dir;
}