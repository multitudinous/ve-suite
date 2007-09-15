#ifndef GETVESUITE_MHeatX_MHeatX_BLOCK_H
#define GETVESUITE_MHeatX_MHeatX_BLOCK_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_MHeatX_MHeatX_BLOCK( void )
{
    unsigned char osgData[ 3561 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,95,0,95,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,134,191,224,153,63,240,76,159,248,38,231,143,127,224,155,159,240,79,159,29,120,235,254,9,243,251,16,120,211,198,222,52,253,136,63,101,15,22,120,199,198,62,44,253,148,62,3,120,143,197,94,44,241,79,136,254,3,120,7,88,241,15,137,124,77,226,29,99,192,51,93,235,222,32,191,213,239,47,46,175,111,110,166,150,230,234,230,234,73,231,145,229,145,152,253,191,255,0,14,157,255,0,130,89,127,210,52,255,0,96,15,252,67,127,217,215,255,0,157,205,31,240,73,223,249,69,151,252,19,79,254,204,3,246,55,255,0,214,117,248,115,95,127,208,7,192,31,240,233,223,248,37,151,253,35,79,246,0,255,0,196,55,253,157,127,249,220,209,255,0,14,157,255,0,130,89,127,210,52,255,0,96,15,252,67,127,217,215,255,0,157,205,125,255,0,69,0,124,1,255,0,14,157,255,0,130,89,127,210,52,255,0,96,15,252,67,127,217,215,255,0,157,205,31,240,233,223,248,37,151,253,35,79,246,0,255,0,196,55,253,157,127,249,220,215,223,244,80,7,192,31,240,233,223,248,37,151,253,35,79,246,0,255,0,196,55,253,157,127,249,220,209,255,0,14,157,255,0,130,89,127,210,52,255,0,96,15,252,67,127,217,215,255,0,157,205,125,255,0,69,0,124,1,255,0,14,157,255,0,130,89,127,210,52,255,0,96,15,252,67,127,217,215,255,0,157,205,31,240,233,223,248,37,151,253,35,79,246,0,255,0,196,55,253,157,127,249,220,215,223,244,80,7,192,31,240,233,223,248,37,151,253,35,79,246,0,255,0,196,55,253,157,127,249,220,215,226,15,252,28,121,255,0,4,246,253,129,126,6,255,0,193,24,255,0,108,159,138,95,5,127,97,239,217,3,224,255,0,196,207,11,255,0,195,60,255,0,194,53,241,23,225,111,236,211,240,95,225,247,142,252,59,253,183,251,85,252,13,240,238,179,253,133,226,239,9,248,42,211,80,210,62,215,225,253,95,85,177,186,251,61,196,127,104,179,212,238,45,101,223,4,242,163,127,87,181,248,3,255,0,7,71,127,202,10,63,110,111,251,182,111,253,108,63,217,246,128,62,255,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,190,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,128,10,40,162,128,10,40,162,128,10,40,162,128,10,40,162,128,10,252,1,255,0,131,163,191,229,5,31,183,55,253,219,55,254,182,31,236,251,95,191,213,248,3,255,0,7,71,127,202,10,63,110,111,251,182,111,253,108,63,217,246,128,62,255,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,190,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,128,10,40,162,128,10,40,162,128,10,40,162,128,10,40,162,128,10,252,1,255,0,131,163,191,229,5,31,183,55,253,219,55,254,182,31,236,251,95,191,213,248,3,255,0,7,71,127,202,10,63,110,111,251,182,111,253,108,63,217,246,128,62,255,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,190,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,128,10,40,162,128,10,40,162,128,10,40,162,128,10,40,162,128,10,252,1,255,0,131,163,191,229,5,31,183,55,253,219,55,254,182,31,236,251,95,191,213,248,3,255,0,7,71,127,202,10,63,110,111,251,182,111,253,108,63,217,246,128,62,255,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,190,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,128,10,43,200,53,159,218,23,224,23,135,62,50,120,79,246,116,241,15,199,15,131,250,23,237,7,227,223,15,220,248,179,192,191,2,117,159,137,126,11,210,254,50,120,211,194,182,112,120,154,234,239,196,190,19,248,97,125,173,38,183,226,63,15,197,109,224,191,24,201,37,237,157,140,214,201,31,132,245,55,105,2,216,93,24,185,255,0,136,191,181,143,236,177,240,127,199,112,124,45,248,183,251,75,126,207,255,0,11,190,38,220,252,63,241,23,197,139,111,135,95,17,126,50,124,58,240,79,142,238,62,22,120,67,71,241,119,136,188,89,241,46,15,8,248,155,196,118,186,132,191,15,244,191,15,252,63,241,237,246,163,172,173,185,211,172,172,252,19,171,221,92,220,197,6,155,122,240,128,123,253,21,243,5,175,237,187,251,23,223,105,223,5,53,139,31,218,239,246,96,188,210,63,105,79,16,106,190,19,253,157,53,75,95,143,191,10,110,52,239,143,222,42,208,188,85,167,248,23,92,240,207,193,75,232,124,88,98,248,169,226,11,47,27,106,218,94,141,119,101,161,53,253,205,190,171,169,91,233,243,70,151,115,71,11,115,255,0,11,127,224,161,63,176,47,199,31,29,232,95,11,126,10,126,220,63,178,7,198,15,137,190,40,254,211,255,0,132,103,225,215,194,223,218,91,224,191,196,15,29,248,139,251,19,71,212,60,69,172,255,0,97,120,71,194,126,53,187,212,53,127,178,120,127,72,213,111,174,190,207,111,39,217,236,244,203,139,169,118,65,12,142,160,31,95,209,92,253,183,139,60,43,121,226,173,103,192,182,126,38,240,253,215,141,188,57,225,255,0,12,248,179,196,62,14,182,214,116,233,252,85,161,120,87,198,154,143,139,52,127,7,120,155,89,240,244,87,38,239,75,240,254,173,171,248,11,199,86,186,101,236,240,165,181,253,207,130,245,104,45,100,150,93,58,241,97,249,3,197,159,240,83,95,248,38,231,128,188,85,226,95,2,248,235,254,10,13,251,16,120,47,198,254,11,241,6,179,225,63,24,248,59,197,159,181,127,192,111,14,120,171,194,126,42,240,230,163,115,163,248,135,195,62,37,240,246,177,227,232,110,244,47,16,88,106,246,119,118,183,182,87,80,197,115,107,115,107,36,19,198,146,163,40,0,251,126,138,249,130,235,246,221,253,139,236,116,239,141,122,197,247,237,119,251,48,89,233,31,179,95,136,52,175,9,254,209,122,165,215,199,223,133,54,250,119,192,31,21,107,190,42,212,60,11,161,248,103,227,93,244,222,44,17,124,43,241,5,239,141,180,157,83,70,180,178,215,90,194,230,227,85,211,110,52,248,99,123,184,100,133,125,127,225,111,197,143,133,159,28,124,9,161,124,82,248,41,241,47,225,255,0,198,15,134,94,40,254,211,255,0,132,103,226,47,194,223,25,120,115,226,7,129,60,69,253,137,172,106,30,29,214,127,176,188,93,225,61,74,239,79,213,254,201,226,13,35,85,177,186,251,61,196,159,103,188,211,46,45,101,217,60,50,34,128,122,5,126,0,255,0,193,209,223,242,130,143,219,155,254,237,155,255,0,91,15,246,125,175,223,234,252,1,255,0,131,163,191,229,5,31,183,55,253,219,55,254,182,31,236,251,64,31,127,255,0,193,39,127,229,22,95,240,77,63,251,48,15,216,223,255,0,89,215,225,205,125,255,0,95,202,23,252,19,215,254,14,60,255,0,130,49,252,14,253,129,127,97,239,130,159,20,191,108,159,248,69,254,38,252,31,253,144,63,102,159,133,191,17,124,51,255,0,12,243,251,85,235,95,240,142,248,239,225,255,0,193,143,5,120,79,197,218,23,246,207,135,126,6,221,233,250,191,217,60,65,164,106,22,255,0,106,177,187,186,179,184,251,63,155,107,113,52,14,146,55,215,255,0,241,20,119,252,16,163,254,143,155,255,0,53,155,246,195,255,0,232,125,160,14,127,254,10,203,226,223,10,167,237,211,251,38,167,130,188,75,225,253,111,246,176,253,157,63,98,15,219,127,246,187,248,15,240,79,194,250,214,157,55,237,15,241,83,197,95,6,63,105,31,248,39,39,198,193,240,35,225,30,129,164,220,203,226,77,35,196,31,26,254,4,254,206,223,181,143,194,223,183,232,218,110,169,115,123,225,45,127,226,36,95,216,158,40,209,180,207,18,248,126,239,207,254,8,254,208,191,0,160,255,0,130,175,120,111,246,169,248,185,241,195,225,7,128,245,127,218,55,246,96,255,0,130,169,248,179,192,80,124,70,248,151,224,189,51,81,240,223,236,95,251,62,124,96,255,0,130,123,124,46,253,158,188,77,225,15,27,120,155,91,181,147,196,191,179,7,143,60,19,251,31,254,214,191,180,159,131,47,108,173,237,60,41,22,149,241,183,226,46,183,161,201,172,217,233,222,38,241,166,181,236,31,241,20,111,252,16,163,254,143,155,255,0,53,155,246,195,255,0,232,125,160,255,0,193,209,191,240,66,142,63,227,57,186,255,0,213,179,126,216,127,79,250,55,223,243,138,0,248,131,246,48,248,195,226,157,11,224,183,252,27,29,123,241,119,246,213,253,136,60,79,240,111,198,94,32,240,55,132,254,12,252,53,240,79,195,77,71,225,127,197,171,63,21,120,87,254,9,95,251,86,254,207,175,225,157,115,226,231,137,255,0,108,191,21,105,95,20,188,65,225,191,138,222,58,240,175,195,111,18,89,105,94,4,240,245,204,223,17,60,125,161,233,134,45,7,82,186,182,240,205,255,0,127,251,5,126,218,255,0,0,172,254,40,93,248,183,226,55,252,20,71,254,9,195,227,111,217,167,193,127,180,255,0,252,22,3,226,55,195,95,217,251,195,41,224,207,17,254,213,63,13,62,38,124,82,255,0,130,138,252,87,212,62,28,254,212,215,62,44,176,248,141,226,251,237,115,225,5,183,236,185,227,15,218,155,92,213,188,111,162,104,191,13,252,61,224,239,133,31,23,111,124,69,227,9,252,77,224,168,181,79,24,248,115,234,15,248,138,59,254,8,81,255,0,71,205,255,0,154,205,251,97,255,0,244,62,210,127,196,81,191,240,66,143,250,62,111,199,254,25,155,246,195,255,0,232,124,255,0,56,160,15,152,62,23,254,217,126,41,240,39,138,190,13,255,0,193,100,126,43,252,75,253,151,252,41,251,23,254,218,159,23,236,190,23,220,248,206,243,246,212,212,111,124,85,63,192,47,218,11,81,240,15,194,255,0,217,111,224,221,199,236,235,226,79,131,186,63,133,62,28,252,96,248,31,226,159,134,150,254,44,248,151,3,124,112,241,174,139,240,242,231,199,95,182,71,136,252,59,160,79,115,227,53,178,240,239,232,255,0,197,239,248,101,139,47,248,42,111,236,165,251,46,107,223,240,160,45,63,225,109,254,192,31,240,85,79,248,73,63,103,77,92,252,58,183,255,0,133,155,255,0,13,17,251,69,126,194,63,17,60,107,253,185,240,138,243,31,240,154,127,194,115,255,0,10,247,246,151,215,117,79,180,105,247,63,240,147,127,194,15,227,173,78,239,237,191,217,154,252,240,120,7,252,69,27,255,0,4,40,255,0,163,230,255,0,205,102,253,176,255,0,250,31,41,7,252,29,27,255,0,4,40,255,0,163,229,29,255,0,230,217,127,108,49,248,255,0,201,190,250,255,0,58,0,224,63,108,127,218,55,246,89,214,126,59,124,120,241,255,0,132,62,44,124,63,159,246,56,253,148,254,32,248,51,195,127,240,93,207,13,105,63,28,126,29,105,254,14,248,175,253,185,224,253,87,225,199,194,31,0,235,223,3,223,78,159,87,248,153,241,3,195,62,46,178,248,117,105,241,46,125,59,93,240,143,252,44,239,2,120,2,127,217,194,77,19,246,145,241,38,132,255,0,7,190,28,126,255,0,124,45,248,177,240,179,227,143,129,52,47,138,95,5,62,37,252,63,248,193,240,203,197,31,218,127,240,141,124,69,248,91,227,47,14,252,64,240,39,136,191,177,53,141,67,195,186,207,246,23,139,188,39,169,93,233,250,183,217,60,65,164,106,182,55,95,103,184,147,236,247,154,101,197,172,187,39,134,68,95,196,31,248,138,55,254,8,81,255,0,71,203,156,127,213,178,254,216,124,118,207,63,179,239,189,3,254,14,141,255,0,130,20,99,254,79,155,255,0,53,155,246,196,255,0,232,125,160,15,223,234,252,1,255,0,131,163,191,229,5,31,183,55,253,219,55,254,182,31,236,251,71,252,69,29,255,0,4,40,255,0,163,230,255,0,205,102,253,176,255,0,250,31,107,242,3,254,11,213,255,0,5,234,255,0,130,78,254,218,63,240,73,239,218,179,246,105,253,154,63,106,207,248,89,95,27,126,36,255,0,194,140,255,0,132,43,193,95,240,163,63,105,47,7,127,109,127,194,29,251,73,252,29,241,255,0,137,63,226,163,241,255,0,193,221,43,72,211,190,207,225,47,10,235,215,127,233,119,240,121,223,96,242,45,252,219,153,97,134,64,15,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 3561; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

