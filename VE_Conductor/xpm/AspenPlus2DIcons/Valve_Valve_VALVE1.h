#ifndef GETVESUITE_Valve_Valve_VALVE1_H
#define GETVESUITE_Valve_Valve_VALVE1_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Valve_Valve_VALVE1( void )
{
    unsigned char osgData[ 2406 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,33,0,49,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,241,31,217,87,192,191,8,63,97,255,0,30,220,217,252,100,253,128,127,100,15,219,3,224,55,141,60,123,101,226,111,136,186,111,198,95,217,159,225,15,196,31,142,254,15,211,174,180,219,235,31,17,95,252,16,248,159,227,29,6,71,146,114,247,158,22,187,62,27,215,165,189,209,175,231,240,188,150,182,23,94,17,188,215,181,143,16,55,245,123,251,37,126,205,223,240,66,223,219,95,225,205,135,196,47,129,63,176,103,252,19,203,87,185,77,23,67,213,60,111,240,211,89,253,143,255,0,102,61,51,226,247,194,27,237,122,125,110,202,211,195,159,22,254,31,195,224,153,238,252,25,172,62,167,225,159,19,65,105,52,134,93,47,89,143,66,155,81,240,254,163,171,232,239,109,168,207,224,254,42,253,141,109,53,168,39,31,217,44,18,88,25,38,138,54,142,92,176,142,68,243,124,166,144,153,183,68,197,10,33,4,129,242,97,219,112,249,46,235,246,2,248,183,240,211,198,22,191,21,63,103,143,29,120,247,224,207,196,221,49,236,78,157,226,207,135,250,174,163,225,219,155,171,107,45,107,76,241,43,120,107,196,90,114,199,37,151,141,60,13,117,171,120,115,195,242,106,30,30,214,237,239,188,61,169,182,149,12,122,142,151,115,2,5,63,47,133,207,99,25,114,86,119,141,247,234,111,42,47,236,159,186,223,240,233,223,248,37,151,253,35,79,246,0,255,0,196,54,253,157,127,249,220,81,255,0,14,158,255,0,130,89,127,210,52,255,0,96,15,252,67,127,217,215,255,0,157,207,249,197,112,159,179,71,237,207,227,251,216,237,252,15,251,103,124,59,178,248,71,227,152,226,241,190,183,47,198,63,11,233,211,232,95,179,93,231,135,180,123,149,212,244,43,45,103,82,241,103,140,47,245,47,134,158,43,62,29,125,86,57,127,180,230,186,208,111,36,240,108,151,113,235,214,23,250,238,149,225,136,187,223,218,139,246,207,212,126,30,88,106,254,4,253,156,124,11,117,241,183,227,102,163,161,120,174,219,67,214,44,237,226,213,254,8,252,52,241,142,137,173,159,11,253,143,227,22,191,165,120,130,210,242,243,84,181,212,173,60,77,52,190,25,209,12,154,164,146,120,54,93,51,89,190,240,144,213,116,173,90,79,117,99,176,174,138,174,171,199,217,247,191,225,109,238,99,203,36,237,202,239,232,120,215,199,79,216,135,254,8,139,251,51,120,10,243,226,111,199,191,216,131,254,9,167,240,183,193,86,175,119,105,111,170,248,171,246,74,253,156,109,110,124,65,173,90,104,90,215,137,87,194,126,12,208,161,248,113,38,161,227,191,29,92,232,158,29,215,39,211,244,13,22,214,255,0,90,212,255,0,179,37,143,78,176,186,153,124,186,254,75,127,108,203,175,217,139,246,222,123,159,134,127,178,127,252,19,51,246,65,253,146,191,103,221,80,120,27,82,62,52,183,253,144,126,1,120,67,246,186,241,61,246,158,183,90,142,187,167,223,248,179,193,26,77,213,143,194,63,8,63,136,111,252,54,130,199,67,186,186,213,239,237,188,34,207,125,226,120,180,189,127,85,240,165,191,235,159,140,191,99,207,218,43,246,157,248,143,168,124,96,253,169,124,105,226,143,29,248,162,243,90,212,245,157,11,194,179,95,235,167,225,183,194,141,63,90,182,210,108,238,60,39,240,143,192,115,94,203,103,224,47,14,125,147,195,62,25,142,236,217,184,212,117,147,160,219,234,30,34,191,214,117,150,185,191,184,250,51,193,159,177,69,158,131,107,105,26,233,5,33,183,27,132,59,99,181,141,157,26,93,136,150,225,149,161,141,36,98,220,227,113,57,11,176,128,190,38,47,61,135,55,37,9,46,91,239,213,154,198,148,183,107,67,249,106,255,0,135,115,120,123,254,132,219,127,251,237,255,0,249,38,138,254,182,191,225,150,109,191,232,17,31,254,5,15,254,61,69,82,198,207,77,127,173,63,203,241,14,72,246,54,252,69,251,95,216,233,17,76,63,180,237,193,133,81,164,146,18,100,148,51,148,125,177,52,242,136,137,16,157,204,78,70,210,221,25,107,227,127,137,191,240,84,219,95,10,234,186,103,134,52,59,171,223,16,248,195,196,90,190,159,225,191,9,120,75,195,154,124,250,255,0,138,124,89,226,157,123,80,181,210,244,15,10,120,123,195,186,53,172,183,26,223,137,239,181,43,136,237,172,108,96,73,46,110,110,39,138,40,160,121,164,88,91,242,215,246,96,248,93,251,93,127,193,79,188,106,250,63,236,247,163,157,23,224,174,129,227,173,55,193,159,23,127,104,239,16,155,88,254,28,124,54,221,101,115,171,235,118,122,70,155,46,183,101,168,252,85,241,244,58,46,153,167,203,111,161,232,43,42,218,94,120,171,66,111,17,106,126,29,210,53,120,53,106,254,181,127,96,223,248,38,159,192,79,216,71,64,182,213,188,51,21,231,196,111,218,7,92,240,93,151,132,254,41,126,208,254,45,23,99,196,254,52,95,237,121,124,71,172,88,248,87,195,18,106,151,90,111,194,79,3,220,107,143,97,255,0,18,109,9,98,107,219,95,9,232,7,196,218,143,137,117,125,30,13,105,249,112,185,23,180,188,171,123,176,78,251,106,247,254,186,14,85,159,67,201,191,102,111,217,195,246,189,248,155,57,241,247,237,139,226,219,255,0,132,190,22,185,111,28,104,214,255,0,179,47,128,60,69,18,120,198,242,220,27,29,11,194,222,47,241,159,199,127,134,62,53,219,225,216,164,142,47,19,234,86,250,31,133,230,123,208,183,62,29,191,188,241,77,156,233,174,248,80,247,191,181,119,236,213,251,70,105,214,122,175,197,15,216,235,226,21,204,218,254,141,164,248,143,94,213,191,103,31,29,234,87,154,182,139,241,99,196,151,94,39,183,241,26,219,124,63,248,151,226,239,20,237,248,65,172,182,145,123,226,171,27,61,46,246,59,159,9,92,77,15,135,116,200,27,192,250,93,174,169,170,220,254,151,81,94,250,203,48,74,139,163,236,34,227,222,215,126,183,220,207,158,119,191,51,185,252,178,233,159,240,83,15,19,120,91,226,71,139,62,11,252,99,211,175,126,27,124,97,248,127,173,73,225,207,29,120,7,196,233,107,101,171,248,119,82,17,90,222,217,204,204,141,37,182,169,164,223,105,119,22,247,250,70,165,107,113,38,159,171,105,154,149,150,163,167,220,220,216,93,219,221,205,245,183,134,191,108,155,13,93,34,45,169,194,235,60,139,26,188,227,107,171,200,168,35,0,218,190,223,36,201,185,114,202,172,24,54,236,5,192,253,42,253,175,191,97,223,217,219,246,224,240,110,151,225,79,142,254,19,189,189,212,252,40,190,37,155,225,207,196,79,10,107,55,190,21,248,145,240,203,87,241,78,136,250,46,163,170,248,79,196,118,12,82,234,213,158,61,34,246,125,19,88,182,213,188,49,170,95,248,91,71,185,214,244,61,80,233,118,75,15,242,9,251,94,126,200,159,182,103,252,18,218,123,223,25,248,226,238,79,142,159,178,198,152,124,21,108,191,180,223,132,116,91,79,14,105,186,78,183,226,88,101,179,155,66,248,175,240,188,120,179,85,213,126,27,93,159,23,105,49,216,218,234,241,190,165,225,123,148,241,55,135,237,23,90,180,215,245,113,160,91,120,88,188,134,48,247,168,222,81,211,78,171,250,255,0,135,46,53,94,205,216,254,131,63,225,167,44,255,0,232,41,97,255,0,129,31,253,211,69,127,42,127,240,223,26,63,253,12,122,55,254,12,35,255,0,228,186,43,79,169,191,229,252,188,191,175,248,125,107,154,62,95,127,252,19,248,132,241,199,252,142,158,47,255,0,177,163,196,31,250,118,187,174,94,138,43,234,35,240,252,151,232,115,133,20,81,64,5,20,81,87,210,126,191,168,31,168,20,81,69,114,1,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 2406; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

