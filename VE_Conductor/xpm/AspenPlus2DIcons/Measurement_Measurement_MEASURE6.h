#ifndef GETVESUITE_Measurement_Measurement_MEASURE6_H
#define GETVESUITE_Measurement_Measurement_MEASURE6_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Measurement_Measurement_MEASURE6( void )
{
    unsigned char osgData[ 1381 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,35,0,24,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,134,127,224,156,191,240,76,159,248,38,223,137,127,99,223,134,90,7,142,191,224,159,63,177,7,143,124,109,240,47,95,248,205,251,34,120,199,226,135,139,63,101,15,128,250,239,138,190,51,248,167,246,39,248,231,241,43,246,66,241,15,199,111,18,222,106,254,1,184,188,181,241,7,142,181,127,130,55,158,47,189,176,186,190,213,174,116,203,159,27,73,166,207,174,107,178,218,54,177,125,247,7,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,52,159,176,143,252,82,190,35,253,190,62,5,233,255,0,191,240,143,192,159,219,255,0,226,207,252,34,58,133,239,239,60,71,169,127,195,89,124,43,248,31,255,0,5,23,248,141,255,0,9,37,220,62,93,173,223,216,190,54,254,218,63,20,116,189,19,236,182,150,127,102,240,174,131,160,89,95,255,0,105,106,246,186,142,185,170,254,128,80,7,228,6,169,251,39,254,203,31,177,39,237,217,251,9,252,88,253,154,63,102,159,217,255,0,224,96,248,253,255,0,13,61,251,12,120,215,64,248,23,240,111,225,215,193,127,237,143,248,88,95,7,99,253,181,188,55,241,75,197,58,175,128,60,57,107,255,0,9,135,252,35,191,240,239,125,123,195,54,58,37,221,176,221,255,0,13,7,121,172,219,234,246,63,216,179,105,94,33,43,232,15,248,40,55,252,82,190,29,253,147,254,58,105,227,206,241,111,192,159,219,251,246,60,255,0,132,71,79,189,253,231,135,117,31,248,107,31,138,150,95,240,78,143,136,223,240,146,90,64,35,185,187,251,23,193,47,219,71,226,142,169,162,125,150,242,207,236,254,42,208,116,11,235,239,237,45,34,215,81,208,245,82,128,57,255,0,28,124,55,253,180,126,21,254,215,31,26,62,53,126,203,223,10,255,0,102,15,137,159,10,126,61,124,31,253,159,52,207,22,248,59,226,223,237,65,241,91,246,109,212,116,159,143,191,8,181,191,142,154,63,140,254,45,201,225,239,135,159,177,207,196,189,55,198,254,32,241,47,193,191,20,254,207,62,24,185,215,174,110,244,237,105,180,175,217,227,65,209,110,214,235,74,209,116,53,178,243,255,0,137,127,181,143,252,20,143,225,95,141,63,103,175,2,248,135,246,39,253,136,47,53,111,218,83,227,6,183,240,83,192,183,58,55,252,20,139,227,205,206,157,164,248,171,65,248,3,241,199,246,139,187,212,60,89,53,247,252,18,206,222,91,15,15,183,130,62,0,248,198,214,57,108,225,191,185,58,174,167,166,64,214,137,105,53,213,245,159,234,253,124,1,251,100,127,201,197,127,193,39,127,236,255,0,254,35,255,0,235,172,191,224,165,148,1,243,255,0,237,85,225,31,248,42,111,237,47,251,56,252,104,248,21,109,251,48,126,192,31,15,252,65,241,35,225,254,191,162,120,15,226,148,31,240,81,31,218,43,196,26,207,193,159,138,113,90,157,79,225,39,199,79,8,233,241,255,0,193,45,116,233,227,248,129,224,111,137,186,127,132,252,91,225,203,203,45,75,75,212,116,253,119,193,154,117,254,153,170,105,186,133,173,181,253,185,95,175,244,80,1,95,0,126,217,31,242,113,95,240,73,223,251,63,255,0,136,255,0,250,235,47,248,41,101,20,80,7,223,244,81,69,0,127,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 1381; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

