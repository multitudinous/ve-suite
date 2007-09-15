#ifndef GETVESUITE_Measurement_Measurement_MEASURE4_H
#define GETVESUITE_Measurement_Measurement_MEASURE4_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Measurement_Measurement_MEASURE4( void )
{
    unsigned char osgData[ 1655 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,37,0,35,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,166,110,255,0,103,191,128,95,240,80,159,218,227,227,95,196,143,218,27,224,119,194,15,218,83,246,123,253,150,188,63,165,126,202,223,179,206,149,241,187,225,167,130,254,39,120,46,207,227,237,198,183,168,120,235,246,231,248,137,240,230,199,93,209,117,29,19,198,158,31,123,155,95,217,123,225,180,250,189,220,247,58,215,133,60,127,251,43,252,83,240,117,182,157,225,166,79,17,77,226,207,96,255,0,135,78,255,0,193,44,191,233,26,127,176,7,254,33,191,236,235,255,0,206,230,143,248,38,95,252,74,63,100,239,15,124,60,241,55,238,190,61,124,42,248,129,241,119,193,191,182,4,19,127,161,221,106,255,0,182,78,167,241,39,196,159,17,127,105,95,139,54,158,29,151,201,159,194,223,15,254,39,252,77,241,230,179,241,107,192,86,210,105,158,31,130,239,225,199,199,159,7,235,58,79,134,188,61,161,106,154,86,145,105,247,253,0,124,1,255,0,14,157,255,0,130,89,127,210,52,255,0,96,15,252,67,127,217,215,255,0,157,205,120,255,0,199,239,248,36,95,236,95,169,124,27,248,133,47,236,157,251,23,126,196,31,0,255,0,107,15,15,120,126,111,28,254,202,127,27,252,53,251,57,252,41,248,105,168,252,33,253,168,62,30,79,111,227,191,217,227,226,77,207,139,124,1,240,214,93,74,15,15,232,191,24,252,57,224,173,67,86,177,251,30,169,167,235,26,85,133,238,143,172,232,154,238,141,127,127,163,223,126,175,210,119,253,63,145,160,15,32,248,3,241,175,194,191,180,95,193,175,135,191,26,188,29,97,226,13,11,72,241,247,135,225,212,238,252,29,227,59,93,59,75,248,133,240,219,197,86,115,220,104,254,58,248,75,241,83,195,218,94,171,125,23,131,254,48,120,67,198,218,111,136,124,49,226,253,5,174,230,185,208,60,77,225,61,87,69,188,97,119,97,58,169,95,204,23,140,254,0,255,0,193,125,254,51,252,66,248,169,241,123,254,8,253,251,110,254,204,31,179,23,252,19,191,226,135,197,255,0,139,222,63,248,9,240,203,227,143,132,252,55,226,111,26,107,122,143,139,190,38,120,175,94,248,219,241,223,69,241,70,175,251,33,252,71,111,21,124,32,248,171,251,65,223,252,86,248,163,240,247,85,181,241,134,165,165,106,94,1,248,195,225,155,237,22,215,67,210,46,44,116,13,44,160,15,219,239,28,193,241,191,246,89,253,168,62,36,252,77,248,47,251,47,124,96,253,168,254,14,126,214,94,31,208,252,115,241,107,194,191,5,60,91,251,53,120,95,197,95,9,63,106,15,132,62,31,240,79,194,43,79,137,55,231,246,159,253,160,60,18,190,50,240,255,0,196,127,217,239,76,248,93,160,75,99,162,106,150,250,127,131,110,127,99,187,75,244,209,53,13,75,226,78,185,170,217,244,31,240,217,31,180,87,253,34,123,246,255,0,255,0,195,143,255,0,4,178,237,244,255,0,130,150,87,223,244,80,7,230,7,195,63,248,40,215,196,255,0,140,30,28,212,124,89,240,235,254,9,117,251,127,248,139,195,250,71,196,15,139,31,11,117,13,67,254,19,63,248,38,86,145,246,127,29,252,13,248,167,227,47,130,159,20,180,47,178,235,191,240,81,187,89,229,254,203,248,155,240,255,0,197,218,103,218,163,137,172,239,127,178,62,219,167,92,93,233,247,22,183,83,31,20,191,105,111,219,123,226,63,129,53,223,134,255,0,5,63,224,158,223,181,255,0,236,255,0,241,55,226,71,246,103,195,191,12,254,208,255,0,20,188,107,255,0,4,212,241,7,129,63,103,63,248,78,245,157,63,194,154,207,237,23,174,248,51,194,127,183,15,139,245,15,137,127,240,128,248,127,87,213,124,95,107,225,27,127,15,222,127,194,97,121,224,203,127,12,75,62,153,6,173,38,171,101,232,31,240,77,63,249,55,95,136,223,246,127,255,0,240,86,47,253,122,111,237,145,95,127,208,7,159,252,39,248,91,224,79,129,223,11,62,26,124,20,248,91,161,127,194,47,240,203,224,255,0,195,255,0,6,252,45,248,117,225,159,237,61,99,91,255,0,132,119,192,159,15,252,57,166,248,79,194,58,23,246,207,136,181,11,189,67,87,251,39,135,244,141,62,223,237,87,215,119,87,151,31,103,243,110,174,38,157,222,70,43,208,40,160,2,138,40,160,15,128,63,224,154,127,242,110,191,17,191,236,255,0,255,0,224,172,95,250,244,223,219,34,190,255,0,162,138,0,40,162,138,0,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 1655; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

