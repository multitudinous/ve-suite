#include "StdAfx.h"
#include "dynparser.h"

DynParser::DynParser()
{
	;
}

DynParser::~DynParser()
{
	;
}

void DynParser::ParseFile(const char * dynFile)
{	
	std::ifstream inFile( dynFile );
	std::ofstream output("output.txt");

    std::string temp;

	//find blocks
    /*while(	temp.compare(0, 8, "  BLOCKS", 0, 8) )
    {
		std::getline(inFile, temp);
        if (inFile.eof())
	        break;
    }

	//loop over blocks and process
	while( !temp.compare(0, 8, "  BLOCKS", 0, 8) )
	{
        std::stringstream tokenizer(temp);
		std::string blockname;
		std::string as;
		std::string type;
		
		tokenizer >> blockname;
		output << blockname << std::endl;
        int  startpos = blockname.find_first_of("\"");
        int  endpos = blockname.find_last_of("\"");
		blockname = blockname.substr( startpos +1, endpos - startpos - 1 ); 
		tokenizer >> as;
		tokenizer >> type;

		output << startpos<<" "<<endpos<<std::endl;
		output << blockname << std::endl;
		//output << as << std::endl;
		output << type << std::endl;
		
		//BlockInfoList["_main_sheet"][blockname].type = type;

        std::getline(inFile, temp);
        if (inFile.eof())
	        break;
	}

	//look for streams
    while(	temp.compare(0, 9, "  STREAMS", 0, 9) )
    {
        std::getline(inFile, temp);
        if (inFile.eof())
	        break;
    }
	
	//loop over streams and process
	while( !temp.compare(0, 9, "  STREAMS", 0, 9) )
	{
		std::stringstream tokenizer(temp);
		std::string streamname;
		std::string as;
		std::string type;
		
		tokenizer >> streamname;
		output << streamname << std::endl;
        int  startpos = streamname.find_first_of("\"");
        int  endpos = streamname.find_last_of("\"");
		streamname = streamname.substr( startpos +1, endpos - startpos - 1 ); 
		tokenizer >> as;
		tokenizer >> type;

		output << startpos<<" "<<endpos<<std::endl;
		output << streamname << std::endl;
		//output << as << std::endl;
		output << type << std::endl;
        
		std::getline(inFile, temp);
        if (inFile.eof())
	        break;
	}
	
	//look for connections
    while(	temp.compare(0, 9, "  Connect", 0, 9) )
    {
        std::getline(inFile, temp);
        if (inFile.eof())
	        break;
    }

	//loop over connections and process
	while( !temp.compare(0, 9, "  Connect", 0, 9) )
	{
		std::stringstream tokenizer(temp);
		while( !tokenizer.eof() )
		{
			std::string token;
			tokenizer >> token;

			//find block and ports
			if( !token.compare(0, 6, "BLOCKS", 0, 6) )
			{
				output << token << std::endl;
			}
			//find streams
			else if ( !token.compare(0, 7, "STREAMS", 0, 7) )
			{
				output << token << std::endl;
			}
		}
        std::getline(inFile, temp);
        if (inFile.eof())
	        break;
	}
	
	//look for PFS Objects
    while(	temp.compare(0, 16, "# of PFS Objects", 0, 16) )
    {
        std::getline(inFile, temp);
        if (inFile.eof())
	        break;
    }

    std::getline(inFile, temp); //throw out PFS entry
    std::getline(inFile, temp); //throw out size

	//loop over streams and populate info
	
	output.close();
*/
	////////////////////////////////////////////////////
	///////////////////////////////////////////////////
	///////////////////////////////////////////////////
    //Open file streams	

    std::map< std::pair< std::string, std::string >,
        std::vector< double > > lutMap;
    std::map< std::pair< std::string, std::string >,
        std::vector< double > >::iterator lutMapIter;
    std::vector< double > lutVector;
    lutVector.resize( 6 );

    lutMap = GetAspenPlusLUT();
    std::string discard;

    std::map< std::string, std::pair< unsigned int, unsigned int > > imageData;
    imageData = GetAspenIconData();

    //
    // Block Info
    //


	//find blocks
    while(	temp.compare(0, 8, "  BLOCKS", 0, 8) )
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
		std::string as;
		std::string type;
		
		tokenizer >> blockname;
		output << blockname << std::endl;
        int  startpos = blockname.find_first_of("\"");
        int  endpos = blockname.find_last_of("\"");
		blockname = blockname.substr( startpos +1, endpos - startpos - 1 ); 
		tokenizer >> as;
		tokenizer >> type;

        std::getline(inFile, temp);

        if(type.find("Hierarchy") == std::string::npos)
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
        }
        count++;
    }

    //The following block contains the network data
    /*std::streampos beforeNetwork;
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
	*/

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
        std::string annotation;
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
            BlockInfoList[sheetIter->first][tempBlockId].icon = tempIcon;
            BlockInfoList[sheetIter->first][tempBlockId].scale = scale;

            //find offset
            float left=0, right=0, bottom=0, top=0; //coords
            float widthOffset = 0;
            float heightOffset = 0;
            output<<BlockInfoList[sheetIter->first][tempBlockId].type+" "+
                BlockInfoList[sheetIter->first][tempBlockId].icon<<std::endl;
            std::pair< std::string, std::string >
                blockKey( BlockInfoList[sheetIter->first][tempBlockId].type,
                BlockInfoList[sheetIter->first][tempBlockId].icon );
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
                "."+BlockInfoList[sheetIter->first][tempBlockId].icon+
                ".JPG"].first;
            float height = 
                imageData[BlockInfoList[sheetIter->first][tempBlockId].type+
                "."+BlockInfoList[sheetIter->first][tempBlockId].icon+
                ".JPG"].second;
            iconLocations[sheetIter->first][ tempBlockId ] =
                std::pair< float, float >( scaledXCoords -
                ( width * widthOffset * 
                BlockInfoList[sheetIter->first][tempBlockId].scale),
                scaledYCoords - ( height * heightOffset * 
                BlockInfoList[sheetIter->first][tempBlockId].scale) );
            count++;
        }
        std::cout<<"Finished Reading Block Info"<<std::endl;
	}
}
