#ifndef GETVESUITE_Cyclone_Cyclone_ICON_H
#define GETVESUITE_Cyclone_Cyclone_ICON_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Cyclone_Cyclone_ICON( void )
{
    unsigned char osgData[ 3141 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,63,0,59,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,254,40,175,227,203,246,7,255,0,130,33,126,193,191,22,127,97,95,216,179,226,159,139,254,18,254,207,186,143,139,126,37,254,201,159,179,151,196,15,20,106,26,215,128,252,21,121,172,223,248,143,198,95,7,188,29,226,45,114,243,86,187,186,209,94,91,173,78,93,79,81,186,121,228,149,154,71,150,71,103,102,98,77,126,100,127,193,104,191,224,154,159,178,95,236,141,227,239,216,214,203,225,111,194,111,130,182,246,127,19,35,248,254,124,79,105,225,207,1,248,62,43,107,195,224,233,190,7,141,24,234,144,90,232,234,183,5,7,138,245,97,15,152,27,111,157,54,220,110,108,248,209,206,240,210,196,79,12,161,46,120,54,175,211,71,102,106,169,73,165,46,143,254,7,249,159,232,137,95,205,151,252,20,56,127,198,216,254,25,123,254,200,159,10,251,19,156,124,106,253,162,48,63,51,95,130,127,178,247,236,133,251,39,120,128,232,131,95,253,151,255,0,103,125,111,205,150,208,74,53,143,130,159,13,117,47,52,52,177,6,18,125,183,195,47,188,16,78,115,156,230,191,123,190,11,127,193,56,127,224,158,90,164,22,167,83,253,131,127,99,29,68,176,139,113,191,253,151,126,8,93,150,206,51,184,220,120,25,179,92,89,134,111,135,157,10,148,37,78,113,115,210,234,218,107,242,8,83,119,79,77,58,31,162,255,0,180,80,255,0,140,110,211,51,198,52,123,178,115,142,128,14,190,217,207,160,235,159,74,249,247,254,8,85,255,0,38,255,0,251,78,231,169,253,180,252,119,158,189,127,225,72,126,207,99,161,233,210,187,191,18,127,193,46,127,224,153,144,120,102,11,136,63,224,157,127,176,164,51,180,76,90,104,191,100,111,128,17,202,78,58,153,19,225,240,36,254,53,249,145,241,251,254,9,235,251,2,104,214,58,171,233,31,176,239,236,127,165,60,118,215,45,27,105,191,179,71,193,123,22,141,150,25,10,178,53,175,130,144,169,4,2,8,233,138,243,50,172,195,11,133,155,124,245,43,57,105,170,74,215,183,155,185,165,72,202,75,84,162,151,252,3,250,178,162,191,204,191,246,174,253,152,63,102,175,14,216,234,109,225,255,0,217,227,224,110,132,209,173,198,198,209,190,18,248,7,76,100,198,252,108,54,94,31,66,184,192,198,61,43,249,46,248,227,167,105,250,79,197,127,25,233,218,85,141,158,153,167,218,234,54,241,218,216,105,246,176,89,89,219,33,211,108,156,164,22,182,200,169,10,23,118,36,42,129,150,39,169,53,245,216,108,100,49,87,228,139,141,149,245,249,127,153,132,163,203,109,119,63,210,167,224,215,252,18,223,254,11,183,240,99,224,255,0,194,159,131,218,31,196,47,248,39,157,230,139,240,159,225,183,129,126,26,104,247,111,251,69,126,210,214,141,117,165,248,19,194,250,87,133,180,251,151,181,95,216,194,65,108,207,103,165,66,198,48,238,19,118,208,236,6,227,248,243,255,0,5,128,248,53,255,0,5,27,253,159,190,41,126,198,250,103,237,110,223,178,239,141,53,79,27,105,159,180,62,161,240,194,63,132,191,27,126,45,107,118,22,80,120,66,251,224,21,183,141,211,197,87,190,52,253,155,52,89,52,185,230,151,197,254,12,58,120,179,183,212,4,235,105,126,110,26,208,195,110,46,191,209,250,191,142,15,248,58,119,254,75,223,252,18,231,254,197,47,219,171,255,0,79,159,177,37,103,83,5,132,130,169,90,52,34,170,89,187,247,111,86,56,202,87,74,250,104,126,115,126,203,158,37,253,172,97,109,15,251,11,224,175,236,239,169,17,45,175,148,117,127,218,127,226,78,135,188,137,34,42,36,22,159,178,38,161,228,251,224,72,71,96,72,218,127,162,111,217,63,197,63,182,29,255,0,139,60,55,97,241,43,224,87,236,213,225,63,135,83,253,176,248,131,197,94,7,253,172,62,40,252,67,241,166,150,176,233,55,179,105,39,72,240,22,191,251,24,120,98,195,92,243,181,184,244,219,123,159,63,196,186,119,217,109,110,166,188,136,222,77,110,150,55,95,140,31,178,55,222,240,249,238,37,179,193,238,63,123,13,127,70,159,2,63,227,222,204,118,196,95,210,190,63,31,90,58,175,171,194,239,174,183,233,103,185,209,77,106,223,79,235,250,249,159,118,248,167,254,69,59,127,250,226,223,94,157,249,226,191,39,127,105,15,249,7,235,31,245,235,119,255,0,162,36,175,214,47,20,255,0,200,167,109,255,0,92,91,249,87,228,239,237,33,255,0,32,253,99,254,189,110,255,0,244,68,149,228,97,151,191,31,85,250,23,47,133,159,204,159,237,141,255,0,30,26,183,251,183,63,251,61,127,25,31,180,15,252,150,63,29,127,216,78,219,255,0,77,118,21,253,155,254,216,223,241,225,171,127,187,115,255,0,179,215,241,145,251,64,255,0,201,99,241,215,253,132,237,191,244,215,97,95,119,148,108,255,0,195,255,0,200,156,181,58,31,239,43,95,199,7,252,29,59,255,0,37,239,254,9,115,255,0,98,151,237,213,207,97,255,0,19,207,216,147,175,181,127,99,245,249,209,255,0,5,44,255,0,130,105,252,17,255,0,130,151,124,17,139,225,215,196,89,101,240,55,197,79,3,73,171,107,255,0,179,247,237,3,160,105,86,250,143,141,190,11,120,215,82,183,180,142,242,88,172,228,187,181,255,0,132,195,225,174,177,253,151,164,91,248,175,194,151,23,118,214,154,237,166,153,105,113,111,119,164,120,147,72,240,223,137,52,47,98,164,121,225,40,255,0,50,104,133,163,79,177,252,178,254,200,248,7,64,32,255,0,203,107,48,51,199,6,104,64,234,113,159,199,191,78,43,250,52,248,15,147,111,103,129,212,67,239,220,119,227,174,14,62,185,231,138,254,56,180,239,7,252,109,253,141,190,61,248,167,246,96,253,166,188,47,63,130,190,43,248,28,216,94,92,52,55,55,90,223,132,60,117,225,157,98,238,250,207,194,31,20,190,22,248,162,109,50,210,47,23,252,49,215,191,178,245,101,176,212,77,149,173,228,55,154,86,163,162,107,90,118,147,226,157,35,93,208,244,175,219,191,217,231,197,16,95,197,107,44,147,9,18,70,130,228,66,138,60,221,192,152,164,193,100,65,150,121,11,149,97,144,184,96,2,1,159,135,204,112,178,140,154,122,53,255,0,3,254,1,211,9,189,186,63,248,7,244,159,226,159,249,20,173,191,235,131,227,174,73,11,216,158,249,207,108,96,102,191,39,191,105,1,157,59,87,198,115,246,91,161,211,169,242,37,231,142,157,63,207,2,186,189,94,67,23,129,45,36,80,132,196,210,202,11,30,24,199,28,45,130,55,116,220,6,123,243,249,126,75,126,210,94,59,135,68,130,240,73,114,144,48,138,88,212,200,136,145,71,36,195,125,212,177,179,97,221,19,123,41,193,220,165,247,38,224,1,175,51,11,65,185,232,239,202,255,0,200,210,82,113,181,186,159,154,31,182,55,252,120,106,188,113,182,231,32,159,247,243,206,61,199,110,167,211,53,252,101,126,208,39,31,24,252,116,48,167,254,38,118,220,144,9,231,75,176,56,200,235,255,0,214,175,236,7,225,23,236,227,251,64,127,193,78,63,104,117,253,156,255,0,103,35,22,133,105,163,127,102,234,223,26,190,55,235,22,151,218,199,195,255,0,217,255,0,225,166,165,121,168,90,47,139,188,71,4,23,86,255,0,240,148,120,231,88,147,75,214,45,188,35,225,75,91,187,29,71,196,215,246,23,142,247,154,79,135,116,143,21,248,135,195,223,218,47,193,143,248,34,135,252,18,211,224,223,194,223,5,124,50,151,246,30,253,153,254,50,221,120,71,70,77,63,81,248,175,251,67,252,9,248,63,241,163,227,95,196,61,94,105,231,191,214,60,93,241,19,226,63,140,124,11,53,214,183,174,222,234,183,119,147,249,22,235,103,163,233,112,203,22,147,225,253,47,71,208,108,116,221,42,207,238,114,202,19,167,79,158,91,73,127,151,225,161,201,54,157,151,99,245,54,138,40,175,84,131,242,95,254,10,209,255,0,4,194,210,127,224,163,63,11,124,29,170,120,67,197,139,240,235,246,160,248,0,190,51,213,63,103,191,28,106,218,134,174,158,2,190,95,28,47,133,166,241,239,194,143,138,58,86,157,109,118,209,120,23,196,242,120,15,193,97,245,235,11,11,173,123,194,250,151,134,116,221,99,79,131,88,211,34,215,124,35,226,143,230,219,246,70,241,175,140,252,35,241,23,196,95,6,62,45,248,59,95,248,99,241,123,225,135,137,95,193,159,19,254,25,120,176,90,91,120,135,193,254,40,180,179,178,191,146,222,241,116,185,166,178,212,244,187,159,14,234,122,102,171,165,106,214,55,87,186,46,187,164,107,90,110,179,160,234,154,158,143,127,99,127,115,253,216,215,242,185,255,0,5,246,248,233,251,51,126,202,223,182,103,252,19,163,226,183,237,23,172,104,223,14,252,21,241,3,225,103,237,105,225,63,27,248,218,207,192,254,35,215,60,71,226,185,254,24,120,207,246,89,215,126,19,248,119,94,212,190,29,120,106,251,94,212,244,61,9,62,39,124,107,187,210,109,37,45,167,88,92,120,247,89,150,40,227,185,212,165,105,188,220,203,9,28,69,9,73,69,186,176,90,91,119,229,254,69,69,217,219,163,62,198,248,137,127,29,143,194,157,59,80,70,104,16,216,125,171,108,110,176,203,41,251,20,147,20,202,227,51,50,166,49,207,32,117,25,53,248,47,15,194,63,142,63,240,80,175,218,54,79,217,139,246,115,54,112,92,90,219,216,107,223,26,254,49,107,218,117,246,171,240,255,0,246,125,248,107,170,234,26,141,148,62,46,241,172,26,117,253,162,248,151,197,250,180,250,94,183,109,225,47,8,91,222,90,234,190,41,212,52,171,228,107,173,23,195,186,55,138,60,89,225,207,164,63,106,159,248,46,247,252,17,63,197,191,2,33,240,127,194,79,218,167,237,222,46,77,28,192,218,122,252,16,253,173,108,202,222,199,104,169,18,45,231,137,62,16,197,108,24,73,184,6,18,237,200,4,156,96,143,219,15,248,32,231,195,221,99,193,95,240,76,95,128,254,42,241,87,135,108,52,143,24,252,125,215,62,47,126,210,58,191,136,162,26,44,250,231,196,159,8,124,105,248,185,227,79,22,252,0,248,131,226,189,91,72,150,73,117,61,82,231,246,94,184,248,31,103,101,14,167,32,213,116,77,11,65,209,252,51,121,109,166,182,136,186,85,143,143,147,229,211,230,156,241,84,101,77,71,101,36,213,255,0,173,205,106,79,72,168,180,237,212,251,131,246,57,253,142,126,6,126,194,191,3,116,15,128,159,0,188,59,62,151,225,189,58,238,231,196,62,42,241,70,185,45,150,161,227,255,0,138,191,16,181,107,107,27,111,19,124,81,248,159,226,27,29,62,210,63,16,248,227,82,77,51,78,137,218,27,91,61,55,76,211,180,157,59,64,240,254,155,163,248,111,71,209,180,109,63,234,90,40,175,170,73,37,100,172,145,129,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 3141; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

