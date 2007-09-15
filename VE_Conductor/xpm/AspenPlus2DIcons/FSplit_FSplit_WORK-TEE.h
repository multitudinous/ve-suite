#ifndef GETVESUITE_FSplit_FSplit_WORK-TEE_H
#define GETVESUITE_FSplit_FSplit_WORK-TEE_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_FSplit_FSplit_WORK-TEE( void )
{
    unsigned char osgData[ 1407 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,22,0,37,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,253,245,213,255,0,101,111,217,59,246,82,255,0,130,234,252,20,248,207,166,254,206,127,8,124,63,168,254,223,95,179,199,199,127,14,232,94,62,182,248,105,225,59,79,248,68,191,107,127,131,183,255,0,240,154,248,179,93,240,206,179,111,225,160,190,23,248,147,241,47,224,55,197,95,138,201,175,220,197,117,111,168,120,158,223,225,149,235,75,246,194,250,193,155,206,63,97,239,217,211,246,119,253,134,127,224,179,31,180,215,135,254,30,254,207,95,5,62,19,252,62,253,184,127,103,155,239,141,127,179,15,137,188,21,240,239,193,158,24,155,225,195,126,204,94,48,240,183,193,159,218,147,224,215,133,181,125,55,79,182,147,194,254,2,241,46,161,169,252,42,248,139,109,164,105,104,116,105,132,247,23,83,60,15,105,97,110,63,70,255,0,224,168,191,178,231,198,143,218,75,225,15,193,77,127,246,97,212,188,61,160,126,212,31,179,87,237,95,240,3,246,132,248,37,226,63,22,53,196,158,22,210,103,208,188,97,31,130,62,43,143,21,105,86,218,254,150,124,85,225,121,190,5,120,239,226,122,94,232,191,218,22,82,106,200,171,103,107,121,103,125,37,165,220,30,101,255,0,5,70,253,134,190,41,126,208,63,6,191,102,95,248,101,93,99,81,240,159,199,143,217,183,227,23,132,116,127,2,120,202,214,243,194,81,107,126,25,248,35,241,171,194,58,191,236,165,251,74,234,240,222,120,208,165,149,205,254,145,251,63,124,83,241,103,138,109,224,34,89,111,117,95,134,218,124,16,233,250,180,178,38,145,124,1,161,251,8,124,37,253,142,191,98,127,217,214,111,218,47,194,223,9,254,11,126,207,150,95,182,31,198,95,3,107,47,171,252,34,248,29,23,195,152,181,77,39,246,155,248,253,103,240,223,246,39,248,97,125,225,143,7,105,151,119,90,101,181,150,149,241,135,225,94,132,234,235,109,163,88,235,30,35,214,252,67,61,175,135,236,175,245,37,180,253,108,175,207,15,248,40,182,149,167,104,95,179,7,194,109,15,71,179,135,79,210,116,111,219,191,254,9,35,165,105,118,22,203,178,222,199,78,211,191,224,168,191,177,149,165,149,157,186,103,228,134,43,104,98,68,29,149,0,175,208,250,0,40,162,138,0,249,3,246,245,253,181,190,22,127,193,59,127,100,239,138,223,182,47,198,189,3,226,7,138,62,25,124,31,255,0,132,23,254,18,109,11,225,110,149,225,205,111,199,119,223,240,176,62,36,248,59,225,110,141,253,133,166,120,179,197,122,38,159,115,229,120,131,198,218,84,215,95,104,213,45,118,89,219,220,73,23,157,58,71,111,47,243,130,127,224,245,111,248,37,144,255,0,154,7,251,127,246,255,0,154,89,251,58,247,25,255,0,163,170,162,138,0,249,131,246,187,255,0,131,186,255,0,224,155,127,31,190,20,248,79,192,190,14,248,37,251,111,233,186,190,133,251,79,254,196,95,26,239,46,60,77,240,219,224,53,158,157,39,133,127,102,191,219,67,224,23,237,25,227,173,62,218,109,47,246,149,189,149,252,65,119,224,159,133,126,33,181,210,98,104,82,218,125,86,242,202,11,203,187,11,73,39,190,183,250,127,254,35,86,255,0,130,89,127,209,3,253,191,255,0,240,214,126,206,191,253,21,84,81,64,7,252,70,173,255,0,4,178,255,0,162,7,251,127,255,0,225,172,253,157,127,250,42,168,162,138,0,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 1407; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

