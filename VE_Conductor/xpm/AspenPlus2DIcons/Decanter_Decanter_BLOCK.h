#ifndef GETVESUITE_Decanter_Decanter_BLOCK_H
#define GETVESUITE_Decanter_Decanter_BLOCK_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Decanter_Decanter_BLOCK( void )
{
    unsigned char osgData[ 3923 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,95,0,95,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,134,191,224,153,63,240,76,175,248,38,231,143,127,224,155,159,240,79,159,29,120,231,254,9,243,251,16,120,211,198,222,52,253,136,63,101,15,22,120,199,198,62,44,253,148,62,3,120,143,197,62,45,241,79,136,190,3,248,11,88,241,7,137,188,77,226,13,99,192,83,93,235,190,33,191,213,175,46,238,175,111,46,166,150,230,234,230,238,73,231,150,73,93,156,253,191,255,0,14,157,255,0,130,89,127,210,52,255,0,96,15,252,67,127,217,215,255,0,157,205,31,240,73,223,249,69,151,252,19,79,254,204,3,246,55,255,0,214,117,248,115,95,127,208,7,192,31,240,233,223,248,37,151,253,35,79,246,0,255,0,196,55,253,157,127,249,220,209,255,0,14,157,255,0,130,89,127,210,52,255,0,96,15,252,67,127,217,215,255,0,157,205,125,255,0,69,0,124,1,255,0,14,157,255,0,130,89,127,210,52,255,0,96,15,252,67,127,217,215,255,0,157,205,31,240,233,223,248,37,151,253,35,79,246,0,255,0,196,55,253,157,127,249,220,215,223,244,80,7,192,31,240,233,223,248,37,151,253,35,79,246,0,255,0,196,55,253,157,127,249,220,209,255,0,14,157,255,0,130,89,127,210,52,255,0,96,15,252,67,127,217,215,255,0,157,205,125,255,0,69,0,124,1,255,0,14,157,255,0,130,89,127,210,52,255,0,96,15,252,67,127,217,215,255,0,157,205,31,240,233,223,248,37,151,253,35,79,246,0,255,0,196,55,253,157,127,249,220,215,223,244,80,7,192,31,240,233,223,248,37,151,253,35,79,246,0,255,0,196,55,253,157,127,249,220,215,226,15,252,28,121,255,0,4,246,253,129,126,6,255,0,193,24,255,0,108,159,138,95,5,63,97,239,216,255,0,224,255,0,196,223,11,255,0,195,60,255,0,194,53,241,23,225,119,236,211,240,99,225,255,0,142,252,59,253,183,251,85,252,13,240,238,179,253,133,226,239,9,120,42,211,80,210,62,215,225,253,95,85,177,186,251,61,196,127,104,179,212,238,45,101,223,4,210,35,127,87,181,248,3,255,0,7,71,127,202,10,63,110,111,251,182,111,253,108,63,217,246,128,62,255,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,190,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,128,10,40,162,128,10,40,162,128,10,40,162,128,10,40,162,128,10,252,1,255,0,131,163,191,229,5,31,183,55,253,219,55,254,182,31,236,251,95,191,213,248,3,255,0,7,71,127,202,10,63,110,111,251,182,111,253,108,63,217,246,128,62,255,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,190,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,128,10,40,162,128,10,40,162,128,10,40,162,128,10,40,162,128,10,252,1,255,0,131,163,191,229,5,31,183,55,253,219,55,254,182,31,236,251,95,191,213,248,3,255,0,7,71,127,202,10,63,110,111,251,182,111,253,108,63,217,246,128,62,255,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,190,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,128,10,40,162,128,10,40,162,128,10,40,162,128,10,40,162,128,10,252,1,255,0,131,163,191,229,5,31,183,55,253,219,55,254,182,31,236,251,95,191,213,248,3,255,0,7,71,127,202,10,63,110,111,251,182,111,253,108,63,217,246,128,62,255,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,190,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,127,197,183,62,42,179,240,175,137,175,60,11,163,120,127,196,94,54,181,240,254,179,115,224,239,15,248,179,196,218,143,130,252,43,174,248,170,13,58,230,95,15,104,222,38,241,142,143,225,61,126,239,194,126,31,186,213,210,206,11,221,78,215,66,214,174,108,45,167,146,234,13,39,81,150,37,179,152,3,160,162,191,39,255,0,103,63,248,40,23,199,223,30,252,26,253,152,63,105,255,0,218,43,246,98,248,63,240,95,246,105,253,172,252,63,251,55,220,248,11,198,191,10,63,106,127,25,252,126,248,133,224,79,21,126,215,115,248,15,74,253,158,244,111,140,95,12,124,77,251,38,252,62,139,72,240,254,177,227,111,136,190,13,240,149,238,167,225,125,115,197,247,58,63,137,188,107,163,75,123,164,143,5,175,137,124,107,225,126,3,225,191,252,20,219,227,235,233,210,124,77,253,162,127,102,255,0,217,131,224,239,236,249,224,255,0,16,126,222,240,124,109,241,159,128,191,110,95,26,124,97,248,201,240,175,225,151,252,19,171,197,63,25,254,23,252,124,248,253,166,254,206,186,223,236,85,224,237,75,226,223,193,251,111,140,159,12,60,27,160,52,158,23,213,53,15,16,217,167,199,159,8,223,222,248,122,38,185,187,178,180,0,253,159,162,191,63,252,63,251,74,126,217,54,255,0,23,126,5,248,55,226,135,236,27,255,0,8,159,195,47,143,255,0,16,60,75,160,218,252,75,240,31,237,9,97,241,115,88,253,158,188,29,161,124,19,248,185,241,99,79,190,253,174,124,27,165,124,38,211,244,255,0,134,254,63,212,252,65,224,207,0,248,78,206,211,193,190,38,248,139,224,65,174,248,175,88,182,159,226,100,83,217,248,42,219,226,71,160,120,55,246,197,240,39,140,255,0,108,159,139,31,177,237,141,159,151,175,252,52,248,127,166,248,138,215,196,223,104,214,100,30,37,241,223,135,172,62,26,120,179,227,207,195,161,163,203,225,136,160,209,191,225,9,248,101,251,81,126,194,218,247,246,187,106,115,217,248,147,254,26,171,251,51,70,71,212,60,13,227,8,244,240,15,176,40,207,249,233,239,95,144,31,19,127,224,169,90,207,195,143,7,127,193,81,173,167,248,49,240,254,239,227,215,252,19,115,254,22,15,196,13,51,224,254,165,241,187,199,126,28,240,239,237,25,251,56,252,60,248,19,240,51,246,146,215,126,39,248,55,226,190,161,251,52,52,22,30,63,211,190,25,124,113,240,245,151,137,188,33,164,105,62,43,139,194,30,42,214,252,39,166,120,139,196,150,58,23,143,60,45,226,155,221,255,0,139,31,182,23,252,20,47,225,175,197,111,131,255,0,12,211,246,20,253,152,39,182,248,231,241,127,69,248,9,240,231,197,190,38,255,0,130,130,248,247,66,211,188,67,241,10,215,246,47,241,151,237,113,241,43,196,150,250,95,134,63,96,79,16,93,233,255,0,7,244,173,95,225,55,198,95,4,105,58,166,169,30,157,226,109,119,85,240,85,150,175,115,224,189,11,68,214,160,185,183,0,253,96,162,191,24,60,97,255,0,5,7,253,188,188,63,226,175,29,120,38,207,246,14,253,152,52,207,27,252,58,253,152,63,99,111,143,94,33,240,15,197,63,248,41,59,252,51,241,86,161,241,55,246,199,212,126,41,124,50,240,111,236,179,225,93,122,47,216,207,88,240,94,181,241,131,254,26,87,224,175,142,124,7,162,221,79,227,27,47,15,120,150,227,196,30,16,212,45,117,91,57,124,65,121,165,232,93,7,142,127,224,161,95,181,63,195,251,175,219,215,199,122,207,236,149,251,63,221,126,206,127,240,78,223,136,30,48,211,126,53,248,219,76,253,179,62,34,77,241,175,197,31,11,60,17,251,56,252,41,253,175,53,111,25,124,45,248,45,117,251,16,65,161,107,127,16,36,248,13,241,131,195,66,223,195,90,183,196,45,15,78,127,23,216,223,104,50,120,178,29,26,43,127,21,220,128,126,191,215,224,15,252,29,29,255,0,40,40,253,185,191,238,217,191,245,176,255,0,103,218,253,253,3,31,254,172,127,147,95,128,95,240,116,119,252,160,163,246,230,255,0,187,102,255,0,214,195,253,159,104,3,239,255,0,248,36,239,252,162,203,254,9,167,255,0,102,1,251,27,255,0,235,58,252,57,175,183,188,89,113,226,155,63,10,248,154,239,192,186,47,135,252,69,227,107,95,15,235,55,62,14,240,247,139,60,75,168,248,47,194,186,247,138,160,211,238,102,240,246,141,226,111,24,232,254,19,215,238,252,39,225,251,173,93,109,32,188,212,237,116,45,106,230,194,218,226,75,168,52,157,70,88,150,206,111,229,143,254,9,237,255,0,7,30,127,193,24,254,7,126,192,191,176,247,193,79,138,95,182,79,252,34,255,0,19,126,15,254,200,31,179,79,194,223,136,190,25,255,0,134,121,253,170,245,191,248,71,124,119,240,255,0,224,191,130,188,39,226,237,11,251,103,195,191,3,110,244,253,95,236,158,32,210,53,11,127,181,88,221,221,89,220,125,159,205,181,184,154,7,73,27,235,255,0,248,138,59,254,8,81,255,0,71,205,255,0,154,205,251,97,255,0,244,62,208,7,175,254,205,191,178,39,237,160,223,179,7,236,101,251,20,126,211,158,19,253,152,62,31,124,28,253,146,180,15,216,170,231,90,248,163,240,35,246,133,248,173,241,167,226,103,198,63,21,126,194,26,255,0,194,31,30,252,44,209,244,191,2,252,65,253,148,60,1,165,252,28,240,254,183,241,91,224,231,130,117,127,16,106,115,107,94,52,185,143,195,218,70,171,224,237,59,73,139,82,241,29,167,143,124,35,243,7,195,31,248,37,31,198,205,86,215,246,130,240,231,197,143,217,75,246,0,248,57,226,223,219,15,254,27,255,0,225,255,0,237,29,251,104,252,5,253,163,62,46,248,251,246,142,190,253,156,63,224,160,95,180,111,137,62,60,252,74,248,97,15,195,173,95,246,34,240,22,147,241,99,226,6,151,225,41,60,27,224,143,7,248,191,198,62,42,188,139,193,114,248,114,47,22,217,120,111,85,210,78,177,240,215,196,126,129,255,0,17,70,255,0,193,10,63,232,249,191,243,89,191,108,63,203,254,77,246,143,248,138,55,254,8,81,255,0,71,203,255,0,154,205,251,97,246,255,0,187,125,160,15,209,239,12,219,255,0,193,66,245,29,70,230,247,226,28,63,179,6,135,109,240,175,195,254,44,184,240,150,143,240,163,226,23,143,147,77,253,178,126,33,73,225,109,103,195,222,12,151,227,12,191,16,254,0,234,183,127,177,23,193,245,213,164,143,93,189,240,247,133,46,190,48,248,158,45,83,85,209,161,143,199,247,154,39,131,117,205,31,226,191,231,15,134,127,224,154,127,180,239,195,79,129,63,178,231,197,63,3,105,222,0,215,191,224,164,223,12,254,63,248,123,227,175,198,109,87,198,63,240,80,159,219,73,255,0,99,109,87,199,126,49,241,143,142,111,255,0,109,31,139,159,9,190,10,107,63,6,245,255,0,1,120,35,226,15,198,127,2,124,70,248,219,164,92,105,250,87,193,143,14,217,124,62,159,246,169,241,54,167,225,77,74,75,207,12,105,18,235,135,252,69,29,255,0,4,40,255,0,163,230,255,0,205,102,253,176,255,0,250,31,104,255,0,136,163,191,224,133,31,244,124,223,249,172,223,182,31,255,0,67,237,0,116,31,180,79,252,19,67,227,231,237,87,240,191,254,10,177,224,111,31,71,240,131,225,167,138,127,105,15,139,254,61,248,213,251,13,252,71,248,125,241,171,198,158,43,212,188,45,226,159,20,127,193,58,244,127,248,39,5,222,159,241,243,195,126,44,253,153,237,116,223,10,248,127,196,95,6,252,49,174,218,248,138,45,46,31,28,220,216,105,95,180,71,136,96,240,205,222,155,226,191,6,120,87,199,23,223,126,254,213,95,9,126,62,252,71,248,223,251,0,248,207,225,23,135,62,15,235,30,8,253,157,63,106,15,18,252,107,248,205,117,241,35,226,183,141,60,3,226,152,188,43,226,15,217,171,227,183,236,200,150,31,12,124,57,225,159,130,62,40,180,241,175,136,98,210,127,105,31,20,248,132,197,170,234,254,25,182,107,143,134,218,126,134,46,214,47,17,220,235,126,27,252,226,255,0,136,163,191,224,133,31,244,124,223,249,172,223,182,31,255,0,67,237,31,241,20,111,252,16,159,159,248,206,92,127,221,178,254,216,124,255,0,230,190,208,7,127,251,77,254,194,223,27,62,44,126,223,94,42,253,171,38,253,138,191,96,31,218,87,195,254,27,248,127,251,22,120,119,246,111,241,127,199,79,218,159,226,239,193,207,142,223,7,124,119,251,37,252,103,248,209,251,70,201,241,23,194,250,191,130,127,96,191,27,207,224,31,237,143,137,191,23,60,59,167,223,105,26,71,137,158,207,91,209,254,13,218,127,194,71,38,165,167,248,134,243,195,90,87,159,219,127,193,43,126,42,120,243,226,239,237,25,251,84,124,118,248,43,251,0,107,223,182,102,157,251,95,254,204,255,0,181,151,236,153,251,71,120,119,78,241,30,185,172,120,131,254,25,243,224,159,192,159,129,222,43,248,67,241,23,77,248,139,240,59,80,212,63,103,127,135,254,50,240,255,0,192,223,20,223,105,17,218,235,191,25,127,225,85,235,191,180,139,248,183,195,241,120,135,199,63,10,252,47,226,207,18,175,252,69,29,255,0,4,40,255,0,163,230,255,0,205,102,253,176,255,0,250,31,104,255,0,136,163,127,224,133,31,244,124,223,249,172,223,182,31,255,0,67,237,0,126,238,248,78,231,197,87,158,22,240,205,231,142,180,95,15,248,115,198,215,94,31,209,174,124,99,225,255,0,9,120,155,81,241,167,133,116,47,21,79,167,91,201,226,29,23,195,94,49,214,60,39,160,93,248,179,195,246,186,179,221,195,101,169,221,104,90,45,197,253,180,17,221,79,164,233,178,202,214,112,254,17,127,193,209,223,242,130,143,219,155,254,237,155,255,0,91,15,246,125,163,254,34,141,255,0,130,20,127,209,243,127,230,179,126,216,127,253,15,181,249,3,255,0,5,234,255,0,130,245,127,193,39,191,109,31,248,36,247,237,89,251,52,126,205,31,181,103,252,44,175,141,191,18,191,225,70,127,194,21,224,175,248,81,159,180,151,131,191,182,191,225,14,253,164,254,14,248,255,0,196,127,241,81,248,255,0,224,238,149,164,105,223,102,240,151,133,117,235,191,244,187,248,60,239,176,121,16,121,183,50,195,12,128,31,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 3923; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

