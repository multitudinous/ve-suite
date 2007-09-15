#ifndef GETVESUITE_Decanter_Decanter-V-DRUM_H
#define GETVESUITE_Decanter_Decanter-V-DRUM_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Decanter_Decanter-V-DRUM( void )
{
    unsigned char osgData[ 3605 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,83,0,53,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,215,191,107,191,218,239,224,135,236,65,240,59,196,223,31,126,61,248,142,109,27,194,90,19,71,166,104,154,22,141,109,22,171,227,143,136,254,52,190,182,188,185,240,255,0,195,143,134,222,26,123,168,91,196,254,56,212,134,159,122,208,91,249,176,90,218,90,88,94,106,218,181,230,157,162,105,186,158,167,103,252,151,124,95,255,0,130,240,255,0,193,68,127,105,211,225,219,127,217,179,193,30,13,253,139,188,45,2,233,23,218,174,165,96,254,28,253,161,190,38,107,122,184,95,19,91,95,105,242,120,163,226,87,195,168,60,59,167,120,42,88,245,47,13,180,182,86,158,13,159,86,130,251,195,169,115,15,138,27,79,191,151,77,175,206,239,218,211,246,175,241,199,252,20,163,246,197,248,153,241,71,197,158,52,111,27,252,10,240,23,140,188,125,224,175,217,87,67,210,60,59,226,31,14,248,51,66,248,33,23,137,37,182,240,223,138,52,207,14,120,133,197,205,159,141,124,93,161,233,30,30,213,188,75,168,106,86,209,234,247,55,147,219,105,179,69,167,233,122,38,139,162,233,31,125,254,206,127,3,52,171,168,52,208,182,114,41,253,209,145,164,179,116,9,18,166,209,12,64,198,219,84,249,174,167,156,159,155,121,109,237,159,23,29,152,186,82,148,41,187,40,238,250,154,70,10,201,190,167,198,215,159,12,127,107,191,141,94,27,62,6,248,195,251,81,254,212,255,0,22,190,31,248,147,236,49,248,167,225,215,196,239,218,19,227,23,142,252,13,174,77,165,94,91,234,250,120,214,60,35,227,63,28,234,54,26,186,218,107,186,110,157,168,89,139,171,25,90,59,157,58,214,234,17,21,196,112,203,31,53,105,255,0,4,237,62,9,188,178,241,143,130,172,181,31,7,120,199,194,183,246,62,34,240,167,139,188,47,52,254,25,241,55,133,60,75,163,93,195,127,161,248,147,195,158,35,210,45,109,175,52,77,126,195,84,183,181,186,178,188,180,185,183,185,183,185,180,138,88,46,33,153,17,215,250,127,248,107,251,61,105,114,90,219,168,182,183,138,119,41,44,12,96,69,14,230,37,70,119,134,82,188,16,21,24,190,237,198,54,60,43,97,125,227,196,31,179,45,133,174,153,12,242,89,193,12,83,196,94,119,251,60,91,158,63,45,115,26,29,145,144,11,28,100,57,39,0,116,220,71,206,79,56,171,41,198,243,209,127,192,54,84,244,91,35,249,94,210,62,55,127,193,80,191,103,205,86,247,197,31,13,255,0,110,15,218,91,80,215,110,180,11,173,30,242,63,138,191,16,53,63,218,3,195,203,105,125,123,166,222,65,29,159,131,255,0,104,25,60,87,161,233,154,201,188,211,45,207,246,141,189,133,182,173,5,179,75,109,109,125,29,165,253,236,23,31,177,63,178,15,252,28,94,218,215,196,239,0,124,22,253,189,254,15,248,43,224,122,248,174,93,111,68,187,253,167,124,3,226,173,106,223,224,238,155,226,203,157,117,143,130,99,241,191,195,143,22,217,222,222,124,40,240,5,198,137,42,216,222,248,158,95,23,248,134,194,199,84,181,131,81,213,98,209,124,63,168,93,94,232,61,55,198,255,0,217,255,0,75,75,59,199,138,206,16,142,75,2,32,121,68,209,172,123,0,150,72,144,151,136,196,54,21,99,187,229,108,146,24,168,252,41,253,166,126,10,232,123,239,34,147,79,145,131,125,164,162,189,137,120,226,153,225,147,114,33,117,30,98,72,75,71,140,43,18,225,147,107,18,15,169,131,205,166,218,82,147,146,123,223,94,198,110,10,239,163,63,209,2,138,254,46,127,224,141,255,0,240,90,127,134,31,178,111,195,127,137,191,179,71,252,20,35,246,132,147,194,191,15,62,21,207,224,75,63,217,47,81,214,190,25,252,94,241,239,140,97,240,109,236,30,45,183,241,183,195,75,173,107,225,239,134,117,214,111,2,248,100,233,94,7,255,0,132,110,45,82,27,123,139,11,95,21,220,232,218,125,229,206,137,165,105,154,102,132,87,210,66,172,39,8,205,52,148,150,205,234,98,127,61,63,240,79,95,249,36,95,14,249,255,0,153,71,193,231,183,95,236,253,44,103,158,189,191,42,254,158,191,102,112,4,58,126,7,120,143,24,3,27,186,16,58,14,43,249,133,255,0,130,122,255,0,201,34,248,119,255,0,98,143,131,255,0,244,223,165,215,244,245,251,51,255,0,169,176,255,0,182,95,250,21,124,158,101,252,74,223,226,253,81,209,15,179,242,63,107,190,18,253,253,59,215,201,135,159,109,242,113,254,125,43,233,127,137,18,200,154,36,10,146,74,128,219,171,16,178,48,4,237,199,32,30,122,15,199,53,243,71,194,95,191,167,127,215,24,127,244,57,43,233,63,137,95,242,5,183,255,0,175,101,254,85,242,210,221,127,93,81,208,126,92,124,121,185,185,91,57,130,220,78,163,201,7,2,89,0,201,143,113,60,55,93,196,159,169,205,127,63,223,180,213,229,223,218,110,135,218,174,48,62,212,192,121,242,224,50,91,74,232,71,205,212,56,4,122,17,145,205,126,253,124,123,255,0,143,57,191,235,128,255,0,209,85,252,254,254,211,63,241,243,119,244,188,255,0,210,57,171,217,203,210,237,253,104,99,63,137,159,200,39,252,20,70,234,234,231,226,55,135,133,197,205,197,192,142,47,16,121,98,105,164,148,71,190,109,39,126,193,35,29,185,216,185,199,93,163,61,5,21,7,252,20,47,254,74,62,131,255,0,92,181,239,253,31,165,209,95,119,135,254,13,63,67,148,253,151,255,0,130,122,255,0,201,34,248,119,255,0,98,143,131,255,0,244,223,165,215,244,245,251,51,255,0,169,176,255,0,182,95,250,21,127,48,191,240,79,95,249,36,95,14,255,0,236,81,240,127,254,155,244,186,254,158,191,102,127,245,54,31,246,203,255,0,66,175,148,204,191,137,91,252,95,170,58,33,246,126,71,237,119,194,95,245,154,119,253,113,135,255,0,67,146,190,147,248,149,255,0,32,91,127,250,246,95,229,95,54,124,37,255,0,89,167,127,215,24,127,244,57,43,233,63,137,95,242,5,183,255,0,175,101,254,85,242,178,222,30,191,228,116,31,150,63,30,255,0,227,206,111,250,224,63,244,85,127,63,191,180,207,252,124,221,253,47,63,244,142,106,254,128,190,61,255,0,199,156,223,245,192,127,232,170,254,127,127,105,159,248,249,187,250,94,127,233,28,213,237,101,251,199,250,232,140,103,241,51,248,253,255,0,130,133,255,0,201,71,208,127,235,150,189,255,0,163,244,186,40,255,0,130,133,255,0,201,71,208,127,235,150,189,255,0,163,244,186,43,238,240,255,0,193,167,232,114,159,178,255,0,240,79,95,249,36,95,14,255,0,236,81,240,127,254,155,244,186,254,158,191,102,127,245,54,31,246,203,255,0,66,175,230,23,254,9,235,255,0,36,139,225,223,253,138,62,15,255,0,211,126,151,95,211,215,236,207,254,166,195,254,217,127,232,85,242,153,151,241,43,127,139,245,71,68,62,207,200,253,174,248,75,254,179,78,255,0,174,48,255,0,232,114,87,210,127,18,191,228,11,111,255,0,94,203,252,171,230,207,132,191,235,52,239,250,227,15,254,135,37,125,39,241,43,254,64,182,255,0,245,236,191,202,190,86,91,195,215,252,142,131,242,199,227,223,252,121,205,255,0,92,7,254,138,175,231,247,246,153,255,0,143,155,191,165,231,254,145,205,95,208,23,199,191,248,243,155,254,184,15,127,249,101,95,207,239,237,51,255,0,31,55,95,75,206,223,244,233,53,123,89,126,241,245,253,17,140,254,38,127,31,191,240,80,191,249,40,250,15,253,114,215,191,244,126,151,69,31,240,80,191,249,40,250,15,253,114,215,191,244,126,151,69,125,222,31,248,52,253,14,83,246,203,246,65,209,175,126,25,88,15,133,190,37,209,60,73,225,223,21,124,56,97,224,207,19,248,111,196,218,85,214,141,226,95,12,235,254,13,184,26,62,183,225,207,19,105,58,157,189,181,198,147,226,91,43,237,46,234,218,250,210,107,120,101,182,187,130,72,38,142,25,35,146,52,254,144,127,102,159,17,105,233,14,156,36,50,70,95,203,43,230,24,19,118,221,174,193,3,78,11,96,48,237,245,247,251,163,254,10,175,255,0,4,87,180,253,165,60,75,241,51,246,200,253,150,181,205,123,68,253,176,117,61,51,193,250,142,183,240,235,90,241,22,154,159,12,62,57,143,135,126,25,127,10,197,164,219,93,107,176,164,191,14,190,38,94,248,63,77,240,150,157,167,95,127,106,219,248,82,73,60,21,103,6,169,166,233,179,234,218,183,139,45,191,153,127,3,126,210,94,38,248,59,226,217,254,30,124,88,240,215,140,126,19,124,70,240,250,105,41,226,15,135,159,19,188,59,174,248,7,197,158,31,185,212,52,43,63,21,88,13,107,195,62,36,211,237,175,180,148,159,195,186,190,159,119,111,29,220,48,249,208,234,112,77,110,86,9,85,165,240,51,60,21,87,41,206,43,154,51,237,242,54,132,150,139,102,143,236,163,225,63,138,116,175,46,194,120,231,141,146,56,227,89,247,62,211,17,86,47,243,42,43,17,242,190,61,67,46,211,130,112,62,146,241,175,140,124,61,169,232,214,209,199,115,186,115,109,194,71,52,103,56,85,59,66,132,99,35,5,96,122,46,113,142,167,159,229,243,225,223,237,219,164,32,211,161,159,83,88,190,215,52,178,139,119,112,66,68,208,55,146,211,201,22,99,43,129,230,18,200,231,14,67,16,217,199,180,107,31,183,134,142,218,101,219,73,226,27,121,214,222,210,87,137,22,246,39,40,201,25,59,226,81,10,16,202,7,1,29,75,5,218,185,36,87,204,207,3,81,205,43,91,110,158,134,254,209,89,31,120,252,117,212,180,9,45,39,70,188,144,203,28,78,37,88,229,182,42,129,0,1,75,56,95,225,32,103,3,113,206,223,186,72,252,15,253,166,142,144,211,220,152,254,222,119,25,217,73,54,172,134,54,183,148,185,86,28,150,242,75,108,57,218,88,174,120,226,186,111,140,191,183,70,143,53,141,220,178,106,176,139,91,136,76,237,115,44,235,109,148,181,183,115,119,110,134,64,242,189,214,20,72,153,18,6,243,8,92,18,193,126,122,253,156,63,103,175,218,83,254,10,125,241,75,76,240,23,193,13,31,196,58,23,194,153,53,253,87,78,248,173,251,77,107,62,25,213,110,62,21,252,52,211,116,107,125,7,88,241,6,157,111,171,151,183,183,241,119,196,182,210,252,89,225,182,211,124,39,111,122,186,189,244,218,253,181,238,161,38,157,160,193,168,235,250,111,171,128,193,86,231,73,39,111,248,99,41,73,93,183,161,249,51,31,252,18,155,246,151,255,0,130,154,124,65,241,219,254,202,63,11,117,255,0,138,82,252,22,143,66,31,16,97,182,248,141,240,127,225,233,240,232,248,141,63,136,23,194,102,83,241,71,196,186,90,235,31,108,255,0,132,7,196,165,127,179,222,235,236,227,77,255,0,75,242,76,246,254,105,95,233,189,251,13,254,195,63,4,191,224,159,127,6,47,190,9,124,12,151,198,154,150,137,174,120,243,196,159,19,60,89,226,127,136,90,253,174,191,226,255,0,23,120,211,196,182,218,70,147,62,175,171,75,164,105,26,110,153,167,173,183,133,252,55,225,93,38,218,223,74,210,244,235,111,178,120,106,11,139,152,110,53,75,141,71,81,189,43,236,105,211,112,167,8,185,107,20,96,221,219,125,207,177,235,205,126,41,124,25,248,63,241,203,195,246,126,19,248,215,240,163,225,175,198,31,10,233,218,205,191,136,180,255,0,12,252,82,240,47,133,254,32,248,126,199,196,22,150,90,142,153,107,174,217,232,222,45,210,238,237,173,117,152,180,221,95,86,183,142,233,35,89,210,13,78,226,37,113,28,242,43,20,86,162,63,155,79,248,43,103,252,18,163,246,13,253,147,127,224,159,31,180,7,237,1,251,63,124,19,212,190,31,124,93,240,1,248,84,124,35,226,248,254,50,124,119,241,51,233,7,197,95,27,126,27,120,43,94,198,137,227,47,137,218,142,155,123,246,143,12,120,143,90,181,255,0,73,179,155,202,23,166,104,124,185,227,138,84,254,78,255,0,99,185,46,254,48,254,215,31,178,183,194,95,136,218,150,177,226,63,135,191,20,191,104,239,130,63,14,188,119,225,230,214,181,109,44,107,222,13,241,191,196,223,12,120,103,197,26,51,106,154,37,237,181,238,154,46,180,61,82,250,3,113,105,115,111,117,8,159,204,183,158,41,85,29,74,43,138,180,32,170,65,40,36,180,232,191,153,26,195,99,253,9,62,25,255,0,193,27,63,224,153,63,9,245,235,191,17,120,107,246,69,248,127,175,95,94,233,23,26,44,214,127,22,53,127,29,252,118,240,236,118,151,55,150,23,210,92,217,248,71,227,119,139,124,67,164,233,218,218,205,167,64,145,106,86,246,81,106,48,219,205,115,107,13,210,91,94,93,195,63,233,173,20,87,98,140,98,173,24,168,175,37,99,32,162,138,41,129,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 3605; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

