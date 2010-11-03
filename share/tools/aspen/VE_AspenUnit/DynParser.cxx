#include "StdAfx.h"
#include "DynParser.h"
#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/model/ModelCreator.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <cctype>
#include <cmath>
#include "AspenPlusLUT.h"
#include "AspenDynamicsLUT.h"
#include "AspenIconData.h"

///////////////////////////////////////////////////////////////////////////////
DynParser::DynParser()
{
    try
    {
        XMLPlatformUtils::Initialize();
    }
    catch(const XMLException &toCatch)
    {
        XERCES_STD_QUALIFIER cerr << "Error during Xerces-c Initialization.\n"
            << "  Exception message:"
            << XMLString::transcode(toCatch.getMessage())
            << XERCES_STD_QUALIFIER endl;
        exit(1);
    }
    
    /*ves::open::xml::XMLObjectFactory::Instance()->
    RegisterObjectCreator( "XML",new ves::open::xml::XMLCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->
    RegisterObjectCreator( "Shader",new ves::open::xml::shader::ShaderCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->
    RegisterObjectCreator( "Model",new ves::open::xml::model::ModelCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->
    RegisterObjectCreator( "CAD",new ves::open::xml::cad::CADCreator() );
    */
    dyndoc = new AspenDynamicsInterface::AspenDynamicsInterface();
    workingDir = "";
    redundantID = 0;
}

///////////////////////////////////////////////////////////////////////////////
DynParser::~DynParser()
{
    //XMLPlatformUtils::Terminate();
}

///////////////////////////////////////////////////////////////////////////////
void DynParser::OpenFile(const char * file)
{
    std::string fileName(file);
    std::string dynfExt(".dynf");
    //ParseFile( ( workingDir + fileName + dynfExt ).c_str());
    NewParseFile( ( workingDir + fileName + dynfExt ).c_str());
    CString filename = ( workingDir + fileName + dynfExt ).c_str();
    dyndoc->Open( filename );
}

///////////////////////////////////////////////////////////////////////////////
void DynParser::CloseFile( )
{
    dyndoc->Close();
    dyndoc->Quit();
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
    //streamIds.clear();
    inLinkToModel.clear();
    outLinkToModel.clear();
    linkPoints.clear();
    linkTypes.clear();
    models.clear();
    iconLocations.clear();
    streamPortIDS.clear();
}

///////////////////////////////////////////////////////////////////////////////
void DynParser::SaveFile()
{
    dyndoc->Save();
}

///////////////////////////////////////////////////////////////////////////////
void DynParser::SaveAs(const char * filename)
{
    dyndoc->SaveAs(filename);
}

///////////////////////////////////////////////////////////////////////////////
void DynParser::SetVisibility(bool show)
{
    dyndoc->SetVisibility( show );
}

///////////////////////////////////////////////////////////////////////////////
void DynParser::ReinitDynamics()
{
    dyndoc->ResetSimulation();
}

///////////////////////////////////////////////////////////////////////////////
void DynParser::NewParseFile(const char * dynFile)
{
    std::ifstream inFile( dynFile );
    currentLevelName = "_main_sheet";
    levelCount = 1;
    int i = 0;

    //Global
    ReadHeader( inFile );
    FindNextEntry( inFile );
    //ReadEncrypted( inFile );
    FindSystemData( inFile );
    FindNextEntry( inFile );
    ReadSystemData( inFile );
    FindNextEntry( inFile );
    ReadSystemData( inFile );
    FindNextEntry( inFile );
    
    //acquire all relevant flow sheet info
    bool flowsheet_entry = true;
    while( flowsheet_entry )//flowsheet_entry )
    {
        ReadFlowsheetComponents( inFile );
        FindNextEntry( inFile );
        flowsheet_entry = false;

        //loop over all levels
        while(  i < levelCount )
        {
            ReadConstraints( inFile );
            FindNextEntry( inFile );
            ReadSystemData( inFile );
            FindNextEntry( inFile );
            ReadGraphicsInformation( inFile );
            FindNextEntry( inFile );
            i++;
            
            //is there a new flow sheet entry to account for
            flowsheet_entry = PeekFlowsheet( inFile );
            if( flowsheet_entry )
            {
                break;
            }
        }
    }

    //modify the locations to account for the 2 different coord systems
    //NormalizeForWX();

    //Global
    ////ReadComponentList();
    //ReadPropeties( inFile );
    //ReadOptions( inFile );
    //ReadOptimization( inFile );
    //ReadEstimation( inFile );
    //ReadHomotrophy( inFile );
    //ReadOnlineLinks( inFile );
    //ReadSnapshot( inFile );
}

///////////////////////////////////////////////////////////////////////////////
void DynParser::ParseFile(const char * dynFile)
{    
    std::ifstream inFile( dynFile );
    //std::ofstream output("output.txt");
    //std::ofstream outFile("log.txt");

    std::string temp;

    //Open icon headers create maps    
    std::map< std::pair< std::string, std::string >,
        std::vector< double > > plusLutMap;
    std::map< std::pair< std::string, std::string >,
        std::vector< double > > dynLutMap;
    std::map< std::pair< std::string, std::string >,
        std::vector< double > >::iterator lutMapIter;
    std::vector< double > lutVector;
    lutVector.resize( 6 );
    plusLutMap = GetAspenPlusLUT();
    dynLutMap = GetAspenDynamicsLUT();

    std::string discard;

    std::map< std::string, std::pair< unsigned int, unsigned int > > imageData;
    imageData = GetAspenIconData();

    // Block Info
    //find blocks
    while(    temp.compare(0, 8, "  BLOCKS", 0, 8) )
    {
        std::getline(inFile, temp);
        if (inFile.eof())
            break;
    }

    //Get block id and type
    int count = 0;
    std::string compVer, compID, compName, compLib, compLibName;
    BlockInfo tempBlockInfo;
    std::string hierarchy;
    bool hierFlag = false;
    while( !temp.compare(0, 8, "  BLOCKS", 0, 8) )
    {
        std::stringstream tokenizer(temp);
        std::string blockname;
        std::string tempHolder;
        std::string as;
        std::string type;
        
        tokenizer >> tempHolder;
        //output << tempHolder << std::endl;
        int  startpos = tempHolder.find_first_of("\"");
        int  endpos = tempHolder.find_last_of("\"");
        blockname = tempHolder.substr( startpos +1, endpos - startpos - 1 ); 
        tokenizer >> as;
        tokenizer >> tempHolder;
        type = tempHolder.substr( 0, tempHolder.size() - 1);
        std::transform(type.begin(), type.end(), type.begin(), std::tolower);
        tempBlockInfo.type = type;

        std::getline(inFile, temp);

        if(type.find("hierarchy") == std::string::npos)
            tempBlockInfo.hierarchical = false;
        else
            tempBlockInfo.hierarchical = true;

        //default hidden value for aspen is false
        tempBlockInfo.iconHidden = 0;

        if( blockname.find(".") == std::string::npos )
        {
            std::stringstream tokenizer( blockname );
            tokenizer >> tempBlockInfo.id;  //remove newline
            BlockInfoList["_main_sheet"][tempBlockInfo.id] = tempBlockInfo;
            models["_main_sheet"][tempBlockInfo.id] = redundantID++;
        }
        else
        {
            //parse out the name of the hierarchy block
            std::stringstream tokenizer( blockname );
            tokenizer >> blockname;  //remove newline
            size_t  pos = blockname.find_last_of( "." );
            std::string temp = blockname.substr( 0, pos );
            tempBlockInfo.id = blockname.substr( pos + 1, blockname.size() );
            BlockInfoList[temp][tempBlockInfo.id] = tempBlockInfo;
            models[temp][tempBlockInfo.id] = redundantID++;
        }
        count++;
    }

    //The following block contains the network data
    std::streampos beforeNetwork;

    while(temp.compare(0, 9, "  Connect", 0, 9)!= 0 && !inFile.eof())
    {
        getline(inFile, temp);
    }

    //find the graphics section
    while(temp.compare(0, 9, "  Connect", 0, 9)== 0 && !inFile.eof())
    {
        std::stringstream portTokenizer(temp);
        std::string portToken;
        std::string tempStream;

        while( portTokenizer >> portToken )
        {
            if(portToken.compare(0, 7, "STREAMS", 0, 7)== 0)
            {
                //parse out temp stream
                int  startpos = portToken.find_first_of("\"");
                int  endpos = portToken.find_last_of("\"");
                tempStream = portToken.substr( startpos +1, endpos - startpos - 1 ); 
            }
        }

        //reset tokenizer
        portTokenizer.clear();
        portTokenizer.str(temp);
        while( portTokenizer >> portToken )
        {
            if(portToken.compare(0, 6, "BLOCKS", 0, 6)== 0)
            {
                int  startpos = portToken.find_first_of(".");
                std::string inputOutput =
                    portToken.substr(startpos, portToken.size() - 1);
                std::string blockName;
                if(inputOutput.find("In_") != std::string::npos )
                {
                    int  startpos = portToken.find_first_of("\"");
                    int  endpos = portToken.find_last_of("\"");
                    blockName = portToken.substr( startpos +1,
                        endpos - startpos - 1 );
                    //outLinkToModel[hierName][ tempStream ] = blockName;
                    inLinkToModel["_main_sheet"][ tempStream ] = blockName;
                }
                else if (inputOutput.find("Out_") != std::string::npos )
                {                    
                    int  startpos = portToken.find_first_of("\"");
                    int  endpos = portToken.find_last_of("\"");
                    blockName = portToken.substr( startpos +1,
                        endpos - startpos - 1 );
                    //inLinkToModel[hierName][ tempStream ] = blockName;
                    outLinkToModel["_main_sheet"][ tempStream ] = blockName;
                }
            }
        }

        getline(inFile, temp);
    }
    //beforeNetwork = inFile.tellg();
   
    //Now we have passed the network data so record it
    //std::streampos afterNetwork;

    //afterNetwork = inFile.tellg();

    //go back to the beginning of the network
    //inFile.seekg( beforeNetwork );

    // allocate memory:
    //char* buffer = new char [afterNetwork - beforeNetwork];
    // read data as a block:
    //inFile.read( buffer, (afterNetwork - beforeNetwork) );
    //std::string networkData( buffer );
    //delete [] buffer;

    //build network information
    //CreateNetworkInformation( networkData );
    

    //Loop for Hierarchy
    std::map< std::string,
        std::map< std::string, BlockInfo > >::iterator sheetIter;
    for (sheetIter = BlockInfoList.begin();
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
            //dataHeader = "PFSVData";
            dataHeader = "# of PFS Objects";
        }
        //else
        //{
        //    dataHeader = sheetIter->first + " PFSVData"; 
        //}

        //locate the PFSVData Entry
        while(temp.compare( 0, dataHeader.size(), dataHeader, 0, 
            dataHeader.size() )!= 0 && !inFile.eof())
        {
            getline(inFile, temp);
        }

        //throw out
        //getline(inFile, temp);  //#PFS Objects
        getline(inFile, temp);  //Size
        getline(inFile, temp);  //Block

        //Read graphic blocks
        count =0;
        std::string id, version, icon, flag, section, at, labelAt, scaleMod;
        std::string annotation, parent, parentAttrib;
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
            //getline(inFile, labelAt);
            getline(inFile, temp);
            //check for the optional line "Parent"
            if(temp.compare(0, 6, "Parent", 0, 6)== 0)
            {
                parent = temp;
                getline(inFile, parentAttrib);
                getline(inFile, labelAt);
            }
            else
            {
                labelAt = temp;
            }
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
            std::string iconType;
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

            BlockInfoList[sheetIter->first][tempBlockId].icon = tempIcon;
            BlockInfoList[sheetIter->first][tempBlockId].scale = scale;

            //find offset
            float left=0, right=0, bottom=0, top=0; //coords
            float widthOffset = 0;
            float heightOffset = 0;
            //output<<BlockInfoList[sheetIter->first][tempBlockId].type+" "+
            //    BlockInfoList[sheetIter->first][tempBlockId].icon<<std::endl;
            std::pair< std::string, std::string >
                blockKey( BlockInfoList[sheetIter->first][tempBlockId].type,
                BlockInfoList[sheetIter->first][tempBlockId].icon );
            
            lutMapIter = dynLutMap.find( blockKey );
            //check dynamics header
            if( lutMapIter != dynLutMap.end() )
            {
                lutVector = lutMapIter->second;
                left = lutVector[ 0 ];
                right = lutVector[ 1 ];
                top = lutVector[ 2 ];
                bottom = lutVector[ 3 ];
            }
            else
            {
                lutMapIter = plusLutMap.find( blockKey );
                //check aspen plus header
                if( lutMapIter != plusLutMap.end() )
                {
                    lutVector = lutMapIter->second;
                    left = lutVector[ 0 ];
                    right = lutVector[ 1 ];
                    top = lutVector[ 2 ];
                    bottom = lutVector[ 3 ];
                }
                //if not found
                else
                {
                    left = 0;
                    right = 0;
                    top = 0;
                    bottom = 0;
                }
            }

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
            
            float width =
                imageData[BlockInfoList[sheetIter->first][tempBlockId].type+
                "_"+BlockInfoList[sheetIter->first][tempBlockId].type+
                "_"+BlockInfoList[sheetIter->first][tempBlockId].icon+
                ".xpm"].first;
            float height = 
                imageData[BlockInfoList[sheetIter->first][tempBlockId].type+
                "_"+BlockInfoList[sheetIter->first][tempBlockId].type+
                "_"+BlockInfoList[sheetIter->first][tempBlockId].icon+
                ".xpm"].second;
            iconLocations[sheetIter->first][ tempBlockId ] =
                std::pair< float, float >( scaledXCoords -
                ( width * widthOffset * 
                BlockInfoList[sheetIter->first][tempBlockId].scale),
                scaledYCoords - ( height * heightOffset * 
                BlockInfoList[sheetIter->first][tempBlockId].scale) );
            count++;
        }
        std::cout<<"Finished Reading Block Info"<<std::endl;

        //locate minimum X - used for normalization
        float minX = 10000;
        float minY = 10000;
        std::map< std::string, std::pair< float, float > >::iterator iter;
        for (iter = iconLocations[sheetIter->first].begin();
            iter != iconLocations[sheetIter->first].end();
            iter++)
        {
            float currentX =
                iconLocations[sheetIter->first][ iter->first ].first;
            float currentY =
                iconLocations[sheetIter->first][ iter->first ].second;
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
        std::string streamId, streamVersion, streamFlag, streamType;
        std::string coordinates, tempR, tempR2;
        bool newStream = true, routeOne = false;
        std::pair< float, float > tempCoords;
        int routeCount = 0;
        //std::ofstream tester2 ("tester2.txt"); 

        //contiously read all stream info to the legend or viewport entry
        while( temp.compare( 0, 8, "VIEWPORT", 0, 8 )!= 0 &&
            temp.compare( 0, 6, "LEGEND", 0, 6 )!= 0 &&
            !inFile.eof( ) )
        {
            if( temp.compare(0, 6, "STREAM", 0, 6)== 0 )//find "STREAM" entry
            {
                getline( inFile, streamId );
                //tester2<<streamId<<": ";
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
                            else
                            {
                                std::cout << "ERROR: "<<
                                    routeCount << std::endl;
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
                    //tester2<<" x: "<<scaledX<<" y: "<<scaledY;
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
            //tester2 << std::endl;
        }

        std::cout << "Finished Reading Streams" << std::endl;
/////////////////////////////////////////////////////////////////////////////////////////////////////
        
        //NORMALIZE FOR WX
        float normX = minX;
        float normY = minY;
        //tester2 << "NormX: " << normX << " NormY: " << normY << std::endl;
        //tester2.close( );
        //blocks
        //std::ofstream tester3( "tester3.txt" );
        for(iter = iconLocations[sheetIter->first].begin( );
            iter != iconLocations[sheetIter->first].end( );
            iter++)
        {
            iconLocations[sheetIter->first][ iter->first ].first =
                iconLocations[sheetIter->first][iter->first].first - normX;
            iconLocations[sheetIter->first][iter->first].second =
                iconLocations[sheetIter->first][iter->first].second - normY;
            //tester3 << iter->first << ": x: " <<
            //    iconLocations[sheetIter->first][iter->first].first << 
            //    " y: " << iconLocations[sheetIter->first][iter->first].second
            //    << std::endl;
        }
        //tester3.close();

        //streams
        //std::ofstream tester( "tester.txt" );
        std::map< std::string, std::vector< std::pair< float, float > > >::
            iterator iter2;
        for( iter2 = linkPoints[sheetIter->first].begin( );
            iter2 != linkPoints[sheetIter->first].end( );
            iter2++)
        {
            //tester << iter2->first << ":";
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
                //tester << " x: " <<
                //    linkPoints[sheetIter->first][ iter2->first ][element].
                //    first << " y: " << ( float )
                //    linkPoints[sheetIter->first][ iter2->first ][element].
                //    second;
            }
            //tester<<std::endl;
        }
        //tester.close();

        //create a log file
        //std::cout<<"Writing log."<<std::endl;
        //count = 0;
        //int streamCount = 0;
        //outFile << BlockInfoList[sheetIter->first].size()<<std::endl;
        //while (count < (int)BlockInfoList[sheetIter->first].size())
        //{
        //    outFile << xCoords[count];
        //    outFile << "\t";
        //    outFile << yCoords[count];
        //    outFile << "\n";
        //    count ++;
        //}
        //count=0;
        //outFile << streamCoordList.size()<<std::endl;
        //while(streamCount < (int)streamCoordList.size())
        //{
        //    outFile<<streamCoordList[streamCount].value.size()<<std::endl;
        //    while(count < (int)streamCoordList[streamCount].value.size())
        //    {    
        //        outFile << streamCoordList[streamCount].value[count].first;
        //        outFile << "\t";
        //        outFile << streamCoordList[streamCount].value[count].second;
        //        outFile << "\n";
        //        count++;
        //    }
        //    streamCount++;
        //    count = 0;
        //}
    }
    std::cout<<"Parsing Completed!"<<std::endl;
    inFile.close();
    //outFile.close();
    
    //std::string network = CreateNetwork();
    //std::ofstream netTest("network.txt");
    //netTest << network << std::endl;
    //netTest.close();

    return;
}

///////////////////////////////////////////////////////////////////////////////
void DynParser::CreateNetworkLinks
    ( ves::open::xml::model::NetworkPtr subNetwork, std::string hierName )
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
            streamPortIDS[ iter->first ] =
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
            streamPortIDS[ iter->first ] =
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
            streamPortIDS[ iter->first ] =
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
std::string DynParser::CreateNetwork( void )
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
        tempModel->SetPluginType( "ADUOPlugin" );
        tempModel->SetVendorName( "DYNAMICSUNIT" );
        tempModel->
            SetIconFilename(BlockInfoList["_main_sheet"][blockIter->first].type
            +"_"+BlockInfoList["_main_sheet"][blockIter->first].type+"_"
            +BlockInfoList["_main_sheet"][blockIter->first].icon + ".xpm" );
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
            unsigned int( iconLocations["_main_sheet"][ blockIter->first ].first ),
            unsigned int( iconLocations["_main_sheet"][ blockIter->first ].second ) 
            ) );
        tempModel->
            SetIconHiddenFlag( BlockInfoList["_main_sheet"][blockIter->first].
            iconHidden );

        double minX = iconLocations["_main_sheet"][ blockIter->first ].first;
        double minY = iconLocations["_main_sheet"][ blockIter->first ].second;

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
                    SetPortNumber( streamPortIDS[ streamIter->first ].first );
                tempPort->SetDataFlowDirection( std::string( "input" ) );
                tempPort->
                    GetPortLocation()->SetPoint( 
                        std::pair< unsigned int, unsigned int >( 
                        (linkPoints["_main_sheet"][ streamIter->first ][0].
                        first - minX ), 
                        (linkPoints["_main_sheet"][ streamIter->first ][0].
                        second - minY ) ) );
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
                    SetPortNumber( streamPortIDS[ streamIter->first ].second );
                tempPort->SetDataFlowDirection( std::string( "output" ) );
                tempPort->GetPortLocation()->SetPoint(
                    std::pair< unsigned int, unsigned int >( (
                    linkPoints["_main_sheet"][ streamIter->first ]
                    [linkPoints["_main_sheet"][ streamIter->first ].size()-1].first - minX ),
                    (linkPoints["_main_sheet"][ streamIter->first ]
                    [linkPoints["_main_sheet"][ streamIter->first ].size()-1].second - minY ) ) );
            }
        }

        //recursively parse subsystems of each block
        if( tempModel->GetIconFilename().find("hierarchy") !=
            std::string::npos )
        ParseSubSystem( tempModel, blockIter->first );

        //attach model to top system
        veSystem->AddModel(tempModel);
    }

    /*ves::open::xml::model::SystemPtr
        topSystem( new ves::open::xml::model::System() );
    ves::open::xml::model::ModelPtr
        topModel( new ves::open::xml::model::Model() );
    topModel->SetModelName( "Dynamics_Flowsheet" );
    topModel->SetVendorName( "DYNAMICSUNIT" );
    topModel->SetIconFilename("dynamics");
    topModel->SetIconRotation( 0 );
    topModel->SetIconScale( 1 );
    topModel->SetIconMirror( 0 );
    topModel->SetIconHiddenFlag( 0 );
    topModel->GetIconLocation()->
        SetPoint( std::pair< double, double >( 0, 0 ) );
    topModel->SetSubSystem( veSystem );
    topSystem->AddModel( topModel );
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >
        ( topSystem, "veSystem" ) );*/
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >
        ( veSystem, "veSystem" ) );

    /*ves::open::xml::model::NetworkPtr
        topNetwork( new ves::open::xml::model::Network() );
    // create default state info section
    topNetwork->GetDataValuePair( -1 )->SetData( "m_xUserScale", 1.0 );
    topNetwork->GetDataValuePair( -1 )->SetData( "m_yUserScale", 1.0 );
    topNetwork->GetDataValuePair( -1 )->
        SetData( "nPixX", static_cast< long int >( 20 ) );
    topNetwork->GetDataValuePair( -1 )->
        SetData( "nPixY", static_cast< long int >( 20 ) );
    topNetwork->GetDataValuePair( -1 )->
        SetData( "nUnitX", static_cast< long int >( 200 ) );
    topNetwork->GetDataValuePair( -1 )->
        SetData( "nUnitY", static_cast< long int >( 200 ) );
    topSystem->AddNetwork(topNetwork);*/

    //std::string fileName( "network.txt" );
    //ves::open::xml::XMLReaderWriter networkWriter;
    //networkWriter.UseStandaloneDOMDocumentManager();
    //networkWriter.WriteXMLDocument( nodes, fileName, "Network" );
    
    std::string fileName( "returnString" );
    ves::open::xml::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();
    netowrkWriter.WriteXMLDocument( nodes, fileName, "Network" );
    //std::ofstream myfile( "network.txt" );
    //myfile << fileName;
    //myfile.close();

    return fileName;
}

///////////////////////////////////////////////////////////////////////////////
void DynParser::ParseSubSystem( ves::open::xml::model::ModelPtr model,
                               std::string networkName)
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
        tempModel->SetPluginType( "ADUOPlugin" );
        tempModel->SetVendorName( "DYNAMICSUNIT" );
        tempModel->
            SetIconFilename(BlockInfoList[networkName][blockIter->first].type +
            "_" + BlockInfoList[networkName][blockIter->first].type+"_" +
            BlockInfoList[networkName][blockIter->first].icon + ".xpm");
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

        double minX = iconLocations[networkName][ blockIter->first ].first;
        double minY = iconLocations[networkName][ blockIter->first ].second;

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
                    SetPortNumber( streamPortIDS[ streamIter->first ].first );
                tempPort->SetDataFlowDirection( std::string( "input" ) );

                tempPort->GetPortLocation()->
                    SetPoint( std::pair< unsigned int, unsigned int >
                    ( ( linkPoints[networkName][streamIter->first][0].
                    first - minX ),
                    (linkPoints[networkName][streamIter->first][0].
                    second - minY ) ) );
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
                    SetPortNumber( streamPortIDS[ streamIter->first ].second );
                tempPort->SetDataFlowDirection( std::string( "output" ) );
                tempPort->GetPortLocation()->SetPoint(
                    std::pair< unsigned int, unsigned int >( (
                    linkPoints[networkName][streamIter->first]
                    [linkPoints[networkName][streamIter->first].size()-1].first - minX ),
                    (linkPoints[networkName][streamIter->first]
                    [linkPoints[networkName][streamIter->first].size()-1].second - minY ) ) );
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
void DynParser::SetWorkingDir( std::string dir )
{
    workingDir = dir;
}

///////////////////////////////////////////////////////////////////////////////
//BLOCKS
std::string DynParser::GetModuleParams( std::string modname, bool block )
{
    ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
    //input variables;
    params->SetCommandName((modname+"InputParams").c_str());

    std::vector< std::vector < std::string > > variables =
        dyndoc->GetVariableList( modname.c_str(), block );

    for(int i = 0; i < variables.size(); i++)
    //for(int i = 0; i < 2; i++)
    {
        std::vector<std::string> paramList;

        //name
        paramList.push_back( variables[i][0].c_str() );

        //description
        paramList.push_back( variables[i][1].c_str() );

        //value
        paramList.push_back( variables[i][2].c_str() );

        //units
        paramList.push_back( variables[i][3].c_str() );
        
        //add list to DVP
        ves::open::xml::DataValuePairPtr
            inpParams( new ves::open::xml::DataValuePair() );
        inpParams->SetData("params",paramList);
        params->AddDataValuePair( inpParams );
    }

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
void DynParser::SetValue( std::string modname, std::string paramname,
                         std::string value, bool block )
{
    dyndoc->SetVariableValue( modname.c_str(), paramname.c_str(),
        value.c_str(), block  );
}
///////////////////////////////////////////////////////////////////////////////
void DynParser::ReadHeader( std::ifstream &file )
{
    //version
    std::string version;
    std::getline( file, version  );

    //libraries
    std::string libs;
    std::getline( file, libs );
}

///////////////////////////////////////////////////////////////////////////////
/*void DynParser::ReadEncrypted( std::ifstream &file )
{
    std::vector< std::string > entry;
    std::string temp;

    while( true )
    {
        std::streampos temppos = file.tellg();
        //!ENCRYPTED!
        std::getline( file, temp );
        //size of encrypted data is also available at end of line

        //make sure it is an encrypted packet
        if( temp.compare( 0, 10, "!ENCRYPTED!", 0, 10 ) )
        {
            //move file pointer back
            file.seekg(temppos);

            //exit loop
            break;
        }

        //DATA
        std::getline( file, temp );
        entry.push_back( temp );
        
        //!END ENCRYPTED!
        std::getline( file, temp );
    }
}*/

///////////////////////////////////////////////////////////////////////////////
void DynParser::FindSystemData( std::ifstream &file )
{
    std::string temp;
    std::streampos temppos;

    while( temp.compare( 0, 10, "SystemData", 0, 10) != 0 )
    {
        temppos = file.tellg();
        std::getline( file, temp );
    }
    //move file pointer back
    file.seekg(temppos);
}

///////////////////////////////////////////////////////////////////////////////
void DynParser::ReadSystemData( std::ifstream &file )
{
    std::string entry;
    std::string temp;
    while( temp.compare(0, 7, "EndText", 0, 7) != 0 )
    {
        std::getline( file, temp );

        //throw out comments
        //if( temp.compare( 0, 1, "//", 0, 1 ) != 0 )
        //{
        //    entry.append( temp );
        //}
    }
}

///////////////////////////////////////////////////////////////////////////////
void DynParser::ReadFlowsheetComponents( std::ifstream &file )
{
    std::string temp;
    
    //Check for "flowsheet" entry
    std::streampos temppos = file.tellg();
    std::getline( file, temp );
    if( temp.compare( "FLOWSHEET" ) != 0 )
    {
        file.seekg(temppos);
    }

    while( true )
    {
        std::streampos temppos = file.tellg();
        std::getline( file, temp );
        std::stringstream tokenizer( temp );

        //generate a token vector
        std::vector< std::string > tokens;
        std::string tempToken;
        while( tokenizer >> tempToken )
        {
            tokens.push_back( tempToken );
        }

        //if you have reached constraint entry
        if( tokens.empty() )
        {
            //move file pointer back
            file.seekg(temppos);

            break; 
        }

        //if you have reached constraint entry
        if( tokens[0].compare( "CONSTRAINTS" ) == 0 )
        {
            //move file pointer back
            file.seekg(temppos);

            break;
        }

        BlockInfo tempBlockInfo;
        tempBlockInfo.hierarchical = false;
        tempBlockInfo.iconHidden = 0;

        //handle hierachy entry
        if( tokens.size() <= 2 )
        {
            if( tokens[0].compare( "HIERARCHY" ) == 0 )
            {
                tempBlockInfo.id =
                    temp.substr( temp.find("HIERARCHY") + 10, temp.size() );
                tempBlockInfo.hierarchical = true;
                tempBlockInfo.type = "hierarchy";
                BlockInfoList[currentLevelName][tempBlockInfo.id] = tempBlockInfo;
                models[currentLevelName][tempBlockInfo.id] = redundantID++;
                
                //set the current level to the hierarchy block
                //or append it to the current level value
                if( currentLevelName.compare( 0, 10, "_main_sheet", 0, 10 ) == 0 )
                {
                    currentLevelName = tempBlockInfo.id;
                }
                else
                {
                    currentLevelName = currentLevelName + "." + tempBlockInfo.id;
                }
                levelCount++;
            }
        }
        else if( tokens.size() >= 3 )
        {
            //streams
            //check last 7 characters for "Stream;"
            if( tokens[2].find( "Stream;" ) != std::string::npos )
            {
                ;
            }

            //control signals
            else if( tokens[2].compare( "ControlSignal;" ) == 0 )
            {
                ;
            }

            //ports
            else if( tokens[0].compare( "Connect" ) == 0 )
            {   
                //for some reason there are 2 types of entries for blocks
                //Ex. "Blocks("test") as PetroFrac;"
                //or  ""test" as Mult;"
                //not sure why and with no docs probably will never know
                //get the stream name
                if( temp.compare( 0, 17, "  Connect Blocks(", 0, 17 ) == 0 ||
                    temp.compare( 0, 17, "  Connect BLOCKS(", 0, 17 ) == 0 )
                {
                    std::stringstream portTokenizer(temp);
                    std::string portToken;
                    std::string tempStream;

                    while( portTokenizer >> portToken )
                    {
                        if(portToken.compare(0, 7, "STREAMS", 0, 7)== 0)
                        {
                        //parse out temp stream
                        int  startpos = portToken.find_first_of("\"");
                        int  endpos = portToken.find_last_of("\"");
                        tempStream =
                            portToken.substr( startpos +1,
                            endpos - startpos - 1 ); 
                        }
                    }

                    if( tempStream.empty() )
                    {
                        int streamStart = temp.find("with") + 5;
                        tempStream = 
                            temp.substr( streamStart, temp.size() - streamStart - 1 );
                    }

                    //reset tokenizer
                    portTokenizer.clear();
                    portTokenizer.str(temp);
                    while( portTokenizer >> portToken )
                    {
                        if( portToken.compare(0, 6, "BLOCKS", 0, 6)
                            == 0 )
                        {
                            int  startpos = portToken.find_first_of(".");
                            std::string inputOutput =
                                portToken.substr(startpos, portToken.size() - 1);
                            std::string blockName;
                            if(inputOutput.find("In_") != std::string::npos )
                            {
                                int  startpos = portToken.find_first_of("\"");
                                int  endpos = portToken.find_last_of("\"");
                                blockName = portToken.substr( startpos +1,
                                    endpos - startpos - 1 );
                                inLinkToModel["_main_sheet"][ tempStream ] =
                                    blockName;
                            }
                            else if (inputOutput.find("Out_") !=
                                std::string::npos )
                            {                    
                                int  startpos = portToken.find_first_of("\"");
                                int  endpos = portToken.find_last_of("\"");
                                blockName = portToken.substr( startpos +1,
                                    endpos - startpos - 1 );
                                outLinkToModel["_main_sheet"][ tempStream ] =
                                    blockName;
                            }
                        }
                    }
                }
                else 
                {            
                    int streamStart = temp.find("with") + 5;
                    std::string temp_stream =
                        temp.substr( streamStart, temp.size() - streamStart - 1 );
                
                    //reset tokenizer
                    std::stringstream portTokenizer(temp);
                    std::string portToken;
                    while( portTokenizer >> portToken )
                    {
                        if( portToken.find( "." ) != std::string::npos )
                        {
                            int  period_pos = portToken.find_first_of(".");
                            std::string inputOutput =
                                portToken.substr(period_pos+1,
                                portToken.size() - 1);

                            std::string blockName =
                                portToken.substr( 0, period_pos );
                            
                            if(inputOutput.find("In_") !=
                                std::string::npos )
                            {
                                inLinkToModel[currentLevelName][temp_stream] =
                                    blockName;
                            }
                            else if (inputOutput.find("Out_") !=
                                std::string::npos )
                            {                    
                                outLinkToModel[currentLevelName][temp_stream] =
                                    blockName;
                            }
                        }
                    }
                }
            }
            
            //skip FLOWSHEET entry
            else if( tokens[0].compare( "FLOWSHEET" ) == 0 )
            {
                ;
            }

            else if( tokens[0].compare( "StructureContainer" ) == 0 )
            {
                //loop to "end"
                while( temp.compare( 0, 3, "End", 0, 3 ) != 0 )
                {
                    std::getline( file, temp );
                }
            }
            
            //handle block entries
            else
            {
                //for some reason there are 2 types of entries for blocks
                //Ex. "Blocks("test") as PetroFrac;"
                //or  ""test" as Mult;"
                //not sure why and with no docs probably will never know
                if( temp.compare(0, 9, "  Blocks(", 0, 9) == 0 ||
                    temp.compare(0, 9, "  BLOCKS(", 0, 9) == 0 )
                {        std::stringstream tokenizer(temp);
                    std::string blockname;
                    std::string tempHolder;
                    std::string as;
                    std::string type;

                    tokenizer >> tempHolder;
                    int  startpos = tempHolder.find_first_of("\"");
                    int  endpos = tempHolder.find_last_of("\"");
                    blockname = tempHolder.substr( startpos +1,
                        endpos - startpos - 1 ); 
                    
                    tokenizer >> as;
                    tokenizer >> tempHolder;

                    type = tempHolder.substr( 0, tempHolder.size() - 1);
                    std::transform(type.begin(), type.end(), type.begin(),
                        std::tolower);
                    tempBlockInfo.id = blockname;
                    tempBlockInfo.hierarchical = false;
                    tempBlockInfo.type = type;
                    BlockInfoList[currentLevelName][tempBlockInfo.id] =
                        tempBlockInfo;
                    models[currentLevelName][tempBlockInfo.id] = redundantID++;
                }
                else
                {
                    std::stringstream tokenizer(temp);
                    std::string tempHolder;
                    std::string blockname;
                    std::string as;
                    std::string type;

                    //the blocks name
                    tokenizer >> blockname;
                    tokenizer >> as;

                    //the blocks type in lower case
                    tokenizer >> tempHolder;
                    type = tempHolder.substr( 0, tempHolder.size() - 1);
                    std::transform(type.begin(), type.end(), type.begin(),
                        std::tolower);

                    tempBlockInfo.id = blockname;
                    tempBlockInfo.hierarchical = false;
                    tempBlockInfo.type = type;
                    BlockInfoList[currentLevelName][tempBlockInfo.id] =
                        tempBlockInfo;
                    models[currentLevelName][tempBlockInfo.id] = redundantID++;
                }
            }
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
void DynParser::ReadConstraints( std::ifstream &file )
{
    std::string entry;
    std::string temp;
    while( temp.compare(0, 2, "END", 0, 2) != 0 )
    {
        std::getline( file, temp );
        entry.append( temp );
    }
}

///////////////////////////////////////////////////////////////////////////////
void DynParser::ReadGraphicsInformation( std::ifstream &file )
{
    std::map< std::pair< std::string, std::string >,
        std::vector< double > > plusLutMap;
    std::map< std::pair< std::string, std::string >,
        std::vector< double > > dynLutMap;
    std::map< std::pair< std::string, std::string >,
        std::vector< double > >::iterator lutMapIter;
    std::vector< double > lutVector;
    std::map< std::string, std::pair< unsigned int, unsigned int > > imageData;

    imageData = GetAspenIconData();
    lutVector.resize( 6 );
    plusLutMap = GetAspenPlusLUT();
    dynLutMap = GetAspenDynamicsLUT();

    std::string temp,
        id,
        version,
        icon,
        flag,
        section,
        at,
        labelAt,
        scaleMod,
        annotation,
        parent,
        parentAttrib,
        discard;

    float minX = 10000;
    float minY = 10000;

    //locate the beginning of graphics info
    while( temp.compare(0, 12, "  ActiveTasks", 0, 12) != 0 )
    {
        std::getline( file, temp );
    }
    
    //# of PFS Objects
    while( temp.compare(0, 15, "# of PFS Objects", 0, 15) != 0 )
    {
        std::getline( file, temp );
    }

    std::getline( file, temp ); //"SIZE" entry

    while( true )
    {
        std::getline( file, temp );

        if( temp.compare( 0, 5, "LEGEND", 0, 5) == 0 ||
            temp.compare( 0, 7, "VIEWPORT", 0, 7) == 0 )
        {
            break;
        }

        //BLOCKS
        else if( temp.compare( 0, 5, "BLOCK", 0, 5 ) == 0 )
        {
            //block id
            std::getline(file, id);

            //version
            std::getline(file, version);
            
            //icon
            std::getline(file, icon);

            //flag
            std::getline(file, flag);

            //section
            std::getline(file, section);

            //at
            std::getline(file, at);
            
            //optional entries
            std::getline(file, temp);
            //check for the optional line "Parent"
            if(temp.compare(0, 6, "Parent", 0, 6)== 0)
            {
                parent = temp;
                std::getline(file, parentAttrib);
                std::getline(file, labelAt);
            }
            else
            {
                //label at
                labelAt = temp;
            }
            std::getline(file, temp);
            //check for the optional line "Annotation"
            if(temp.compare(0, 10, "Annotation", 0, 10)== 0)
            {
                annotation = temp;
                std::getline(file, scaleMod);
            }
            else
            {
                scaleMod = temp;
            }            
            
            //icon info
            std::string tempIcon;
            std::stringstream iconTokenizer(icon);
            iconTokenizer >> discard;
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

            //scale and modifier
            float scale;
            int modifier;
            std::stringstream scaleTokenizer(scaleMod);
            scaleTokenizer >> discard; //discard "scale" string
            scaleTokenizer >> scale;   //grab scale
            scaleTokenizer >> discard; //discard "modifier" string
            scaleTokenizer >> modifier;//grab modifier

            //block id    
            std::stringstream idTokenizer(id);
            idTokenizer >> discard;
            std::string tempBlockId; 
            idTokenizer >> tempBlockId;

            BlockInfo tempBlockInfo_2;
            //sort the block id/type vector
            int entryIncr = 0;
            bool entryFound = false;
            std::string iconType;

            BlockInfoList[currentLevelName][tempBlockId].icon = tempIcon;
            BlockInfoList[currentLevelName][tempBlockId].scale = scale;

            //ASPEN DYNAMICS ICON OFFSET
            float left = 0, right = 0, bottom = 0, top = 0; //coords
            float widthOffset = 0;
            float heightOffset = 0;
            std::pair< std::string, std::string >
                blockKey( BlockInfoList[currentLevelName][tempBlockId].type,
                BlockInfoList[currentLevelName][tempBlockId].icon );
                
            lutMapIter = dynLutMap.find( blockKey );
            //check dynamics header
            if( lutMapIter != dynLutMap.end() )
            {
                lutVector = lutMapIter->second;
                left = lutVector[ 0 ];
                right = lutVector[ 1 ];
                top = lutVector[ 2 ];
                bottom = lutVector[ 3 ];
            }
            else
            {
                lutMapIter = plusLutMap.find( blockKey );
                //check aspen plus header
                if( lutMapIter != plusLutMap.end() )
                {
                    lutVector = lutMapIter->second;
                    left = lutVector[ 0 ];
                    right = lutVector[ 1 ];
                    top = lutVector[ 2 ];
                    bottom = lutVector[ 3 ];
                }
                //if not found
                else
                {
                    left = 0;
                    right = 0;
                    top = 0;
                    bottom = 0;
                }
            }

            float iconWidth = right - left;
            float iconHeight = top - bottom;

            //MODIFIER
            if(modifier == 0)
            {
                BlockInfoList[currentLevelName][tempBlockId].rotation = 0.0f;
                BlockInfoList[currentLevelName][tempBlockId].mirror = 0;
                widthOffset = abs(left/iconWidth);
                heightOffset = abs(top/iconHeight);
            }
            else if(modifier == 1)
            {
                BlockInfoList[currentLevelName][tempBlockId].rotation = 0.0f;
                BlockInfoList[currentLevelName][tempBlockId].mirror = 1;
                widthOffset = abs(right/iconWidth);
                heightOffset = abs(top/iconHeight);
            }
            else if(modifier == 2)
            {
                BlockInfoList[currentLevelName][tempBlockId].rotation = 0.0f;
                BlockInfoList[currentLevelName][tempBlockId].mirror = 2;
                widthOffset = abs(left/iconWidth);
                heightOffset = abs(bottom/iconHeight);
            }
            else if(modifier == 3)
            {
                BlockInfoList[currentLevelName][tempBlockId].rotation = 90.0f;
                BlockInfoList[currentLevelName][tempBlockId].mirror = 0;
                widthOffset = abs(top/iconWidth);
                heightOffset = abs(right/iconHeight);
            }
            else if(modifier == 4)
            {
                BlockInfoList[currentLevelName][tempBlockId].rotation = 270.0f;
                BlockInfoList[currentLevelName][tempBlockId].mirror = 0;
                widthOffset = abs(bottom/iconWidth);
                heightOffset = abs(left/iconHeight);
            }
            else if(modifier == 5)
            {
                BlockInfoList[currentLevelName][tempBlockId].rotation = 180.0f;
                BlockInfoList[currentLevelName][tempBlockId].mirror = 0;
                widthOffset = abs(right/iconWidth);
                heightOffset = abs(bottom/iconHeight);
            }
            else if(modifier == 6)
            {
                BlockInfoList[currentLevelName][tempBlockId].rotation = 270.0f;
                BlockInfoList[currentLevelName][tempBlockId].mirror = 2;
                widthOffset = abs(top/iconWidth);
                heightOffset = abs(left/iconHeight);
            }
            else if(modifier == 7)
            {
                BlockInfoList[currentLevelName][tempBlockId].rotation = 270.0f;
                BlockInfoList[currentLevelName][tempBlockId].mirror = 1;
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
                imageData[BlockInfoList[currentLevelName][tempBlockId].type+
                "_"+BlockInfoList[currentLevelName][tempBlockId].type+
                "_"+BlockInfoList[currentLevelName][tempBlockId].icon+
                ".xpm"].first;
            float height =
                imageData[BlockInfoList[currentLevelName][tempBlockId].type+
                "_"+BlockInfoList[currentLevelName][tempBlockId].type+
                "_"+BlockInfoList[currentLevelName][tempBlockId].icon+
                ".xpm"].second;
            iconLocations[currentLevelName][ tempBlockId ] =
                std::pair< float, float >( scaledXCoords -
                ( width * widthOffset * 
                BlockInfoList[currentLevelName][tempBlockId].scale),
                scaledYCoords - ( height * heightOffset * 
                BlockInfoList[currentLevelName][tempBlockId].scale) );
                    
            float currentX =
                iconLocations[currentLevelName][tempBlockId].first;
            float currentY =
                iconLocations[currentLevelName][tempBlockId].second;

            if(currentX < minX)
                minX = currentX;
            if(currentY < minY)
                minY = currentY;
        }
        //STREAMS
        else if( temp.compare( 0, 5, "STREAM", 0, 5 ) == 0 )
        {
            std::string coordinates, 
                tempR, 
                tempR2;

            std::pair< float, float > tempCoords;

            std::string streamId,
                streamVersion,
                streamFlag,
                streamType;

            getline( file, streamId );
            getline( file, streamVersion );
            getline( file, streamFlag );
            getline( file, streamType );

            //Look for Stream type - needed for version 13.2
            while( streamType.compare( 0, 4, "TYPE", 0 , 4) != 0 )
                getline( file, streamType );
            getline(file, temp);
            int routeCount = 0;
            bool routeOne=false;
            bool newStream = true;
                
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
                    getline( file, temp );
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
                        else
                        {
                            std::cout << "ERROR: "<<
                                routeCount << std::endl;
                        }
                        getline( file, temp );
                    }
                }
                else 
                {
                    getline( file, temp );
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
            
            linkTypes[currentLevelName][xy.streamId] = ( xy.streamType );
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
                linkPoints[currentLevelName][xy.streamId].push_back(
                    std::pair< float, float >( scaledX, scaledY ) );    
            }
            // add converted points for wx
            //empty temporary vector
            xy.value.erase(xy.value.begin(),xy.value.end() );
            //empty temporary vector
            tempXY.value.erase(tempXY.value.begin( ),tempXY.value.end( ) );
        }
    }
    
    //locate the end of graphics info
    while( temp.compare(0, 7, "ENDTEXT;", 0, 7) != 0 )
    {
        std::getline( file, temp );
    }
    //throw out "END"
    std::getline( file, temp );
    //throw out empty line
    std::getline( file, temp );

    //NORMALIZE FOR WX
    float normX = minX;
    float normY = minY;

    //blocks
    std::map< std::string, std::pair< float, float > >::iterator iter;
    for(iter = iconLocations[currentLevelName].begin( );
        iter != iconLocations[currentLevelName].end( );
        iter++)
    {
        iconLocations[currentLevelName][ iter->first ].first =
            iconLocations[currentLevelName][iter->first].first - normX;
        iconLocations[currentLevelName][iter->first].second =
            iconLocations[currentLevelName][iter->first].second - normY;
    }

    //streams
    std::map< std::string, std::vector< std::pair< float, float > > >::
        iterator iter2;
    for( iter2 = linkPoints[currentLevelName].begin( );
        iter2 != linkPoints[currentLevelName].end( );
        iter2++)
    {
        for( int element = 0;
            element <
            (int)linkPoints[currentLevelName][ iter2->first ].size();
            element++)
        {
            linkPoints[currentLevelName][ iter2->first ][element].first =
                linkPoints[currentLevelName][ iter2->first ][element].
                first - normX;
            linkPoints[currentLevelName][ iter2->first ][element].second =
                linkPoints[currentLevelName][ iter2->first ][element].
                second - normY;
        }
    }

    //move level name up
    if( currentLevelName.find( "." ) == std::string::npos )
    {
        currentLevelName = "_main_sheet";
    }
    else
    {
        std::string temp_name;
        temp_name = currentLevelName.substr( 0, currentLevelName.find_last_of( "." ) );// -1 );
        currentLevelName = temp_name;
    }
}

////////////////////////////////////////////////////////////////////////////////
bool DynParser::PeekFlowsheet( std::ifstream &file )
{
    std::string temp;
    //while( temp.empty() )
    //{
    //    std::getline( file, temp );
    //}

    std::streampos temppos = file.tellg();
    std::getline( file, temp );
    file.seekg(temppos);

    //if( temp.compare(0, 9, "CONSTRAINTS", 0, 9 ) == 0 )
    //{
    //    return false;
    //}

    if( temp.compare(0, 10, "Properties", 0, 10 ) == 0 )
    {
        return false;
    }
    else
    {
        return true;
    }
}

///////////////////////////////////////////////////////////////////////////////
void DynParser::FindNextEntry( std::ifstream &file )
{
    std::string temp;
    std::streampos temppos;
    while( temp.empty() )
    {
        temppos = file.tellg();
        std::getline( file, temp );
    }
    file.seekg(temppos);
}
///////////////////////////////////////////////////////////////////////////////
void DynParser::AddADVariable( const std::string& var )
{
    std::vector<std::string>::const_iterator pos = 
        std::find( m_adVariables.begin(), m_adVariables.end(), var );  

    if( pos == m_adVariables.end() || m_adVariables.empty() )
    {
        m_adVariables.push_back( var );
    }
}
///////////////////////////////////////////////////////////////////////////////
std::string DynParser::GetADValues( )
{
    if( m_adVariables.empty() )
    {
        return "NULL";
    }

    std::vector< std::pair< std::string, std::string > > nameAndValues;
    //ALL THIS SHOULD BE REPLACED and GetVariableList should be leveraged in
    //a new function that allows for querying a single variable
    //READ VALUES
    for( int i = 0; i < m_adVariables.size(); i++ )
    {
        std::pair< std::string, std::string > nameNValue;
        std::string tempName =
            m_adVariables[i].substr( 0, m_adVariables[i].find(".") );
        std::string tempVar =
            m_adVariables[i].substr( m_adVariables[i].find(".") - 1,
            m_adVariables[i].size() );
        
        std::vector< std::vector< std::string > > list =
            dyndoc->GetVariableList( tempName.c_str(), true );
        
        for( int j = 0; j < list[i].size(); j++ )
        {
            std::string temp = list[j][0];
            if( temp.compare( tempVar ) == 0 )
            {
                nameNValue.first = m_adVariables[i];
                nameNValue.second = list[j][2];
                break;
            }
        }
        nameAndValues.push_back( nameNValue );
    }

    //append the flowsheet name
    ves::open::xml::CommandPtr varAndValues( new ves::open::xml::Command() );
    varAndValues->SetCommandName("ADData");

    //loop over the variables and add as dvps
    for( int i = 0; i < nameAndValues.size(); i++ )
    {
         ves::open::xml::DataValuePairPtr
             entry( new ves::open::xml::DataValuePair() );
        entry->SetData( nameAndValues[i].first, nameAndValues[i].second );
        varAndValues->AddDataValuePair( entry );
    }

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( 
    std::pair< ves::open::xml::XMLObjectPtr, std::string >
    ( varAndValues, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}
///////////////////////////////////////////////////////////////////////////////
bool DynParser::IsADVarsEmpty()
{
    if( m_adVariables.size() == 0 )
    {
        return true;
    }
    else
    {
        return false;
    }
    //return m_adVariables.empty();
}
///////////////////////////////////////////////////////////////////////////////
int DynParser::NumADVars()
{
    //if( m_adVariables.empty() )
    if( m_adVariables.size() == 0 )
    {
        return 0;
    }
    else
    {
        return m_adVariables.size();
    }
}