#ifndef GETVESUITE_RYield_RYield_BLOCK_H
#define GETVESUITE_RYield_RYield_BLOCK_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_RYield_RYield_BLOCK( void )
{
    unsigned char osgData[ 3218 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,95,0,95,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,134,191,224,153,63,240,76,159,248,38,223,143,127,224,155,127,240,79,159,29,120,235,254,9,243,251,16,120,211,198,222,52,253,136,63,101,15,22,120,199,198,62,44,253,148,62,3,120,143,197,94,44,241,87,136,254,3,120,7,88,241,15,137,188,77,226,29,99,192,51,93,235,222,32,191,213,239,47,46,175,111,110,166,150,230,234,230,234,73,231,145,229,118,99,246,255,0,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,52,127,193,39,127,229,22,95,240,77,63,251,48,15,216,223,255,0,89,215,225,205,125,255,0,64,31,0,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,71,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,53,247,253,20,1,240,7,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,52,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,95,127,209,64,31,0,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,71,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,53,247,253,20,1,240,7,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,52,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,95,127,209,64,31,0,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,95,136,63,240,113,231,252,19,219,246,5,248,29,255,0,4,99,253,178,126,41,124,20,253,135,191,100,15,131,255,0,19,124,47,255,0,12,243,255,0,8,207,196,95,133,191,179,79,193,127,135,254,59,240,239,246,223,237,87,240,55,195,186,207,246,23,139,188,39,224,171,77,67,72,251,95,135,245,125,86,198,235,236,247,17,253,162,207,83,184,181,151,124,19,72,141,253,94,215,224,15,252,29,29,255,0,40,40,253,185,191,238,217,191,245,176,255,0,103,218,0,251,255,0,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,248,3,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,0,40,162,138,0,40,162,138,0,40,162,138,0,40,162,138,0,43,240,7,254,14,142,255,0,148,20,126,220,223,247,108,223,250,216,127,179,237,126,255,0,87,224,15,252,29,29,255,0,40,40,253,185,191,238,217,191,245,176,255,0,103,218,0,251,255,0,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,248,3,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,0,40,162,138,0,40,162,138,0,40,162,138,0,40,162,138,0,43,240,7,254,14,142,255,0,148,20,126,220,223,247,108,223,250,216,127,179,237,126,255,0,87,224,15,252,29,29,255,0,40,40,253,185,191,238,217,191,245,176,255,0,103,218,0,251,255,0,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,248,3,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,0,40,162,138,0,40,162,138,0,40,162,138,0,40,162,138,0,43,240,7,254,14,142,255,0,148,20,126,220,223,247,108,223,250,216,127,179,237,126,255,0,87,224,15,252,29,29,255,0,40,40,253,185,191,238,217,191,245,176,255,0,103,218,0,251,255,0,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,248,3,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,0,40,162,190,63,255,0,135,133,126,192,191,240,181,63,225,69,255,0,195,112,254,200,31,240,187,63,225,96,127,194,167,255,0,133,59,255,0,13,45,240,99,254,22,167,252,45,63,248,72,191,225,15,255,0,133,105,255,0,10,247,254,19,95,237,113,241,3,254,18,220,233,127,216,223,99,254,209,254,209,63,97,251,55,218,191,117,64,31,96,81,95,47,221,126,219,191,177,125,134,155,241,175,88,190,253,175,63,102,11,61,39,246,107,241,6,149,225,63,218,51,85,186,248,251,240,166,223,77,248,5,226,173,119,197,87,254,5,209,60,51,241,170,246,95,22,8,190,21,248,130,247,198,218,78,169,163,90,89,107,175,97,115,113,170,233,183,26,124,49,61,220,50,194,190,127,109,255,0,5,54,255,0,130,110,94,120,87,89,241,213,159,252,20,27,246,32,187,240,79,135,60,65,225,159,9,248,135,198,54,223,181,127,192,105,252,45,161,120,167,198,154,119,139,53,143,7,120,107,89,241,12,94,62,54,154,95,136,53,109,35,192,94,58,186,211,44,231,153,46,47,173,188,23,171,79,107,28,177,105,183,141,8,7,220,20,87,199,254,27,255,0,130,132,254,192,222,49,62,0,255,0,132,71,246,225,253,144,60,85,255,0,11,95,226,5,239,194,127,133,163,195,159,180,183,193,125,115,254,22,87,197,61,59,254,16,163,168,124,52,240,7,246,103,141,37,255,0,132,199,226,4,31,240,178,190,29,121,218,54,157,246,157,70,47,248,79,180,93,246,203,253,171,97,231,244,31,26,255,0,109,223,216,191,246,107,241,86,159,224,95,218,47,246,188,253,152,62,0,248,223,86,240,253,175,139,52,175,7,124,107,248,251,240,167,225,95,138,181,47,10,223,234,58,174,143,99,226,107,15,15,120,235,197,150,23,119,158,31,155,86,208,117,203,88,175,35,133,173,164,185,209,174,224,73,76,182,242,170,0,125,65,69,115,254,19,241,103,133,124,123,225,95,12,248,235,192,190,38,240,255,0,141,60,19,227,79,15,232,222,44,240,119,140,124,39,172,233,222,35,240,175,139,60,43,226,61,58,219,88,240,247,137,188,51,226,29,30,230,107,77,123,195,247,250,69,229,157,213,149,237,172,210,219,93,91,93,71,60,18,60,78,172,122,10,0,43,240,7,254,14,142,255,0,148,20,126,220,223,247,108,223,250,216,127,179,237,126,255,0,87,224,15,252,29,29,255,0,40,40,253,185,191,238,217,191,245,176,255,0,103,218,0,251,255,0,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,254,80,127,224,158,191,240,113,231,252,17,143,224,111,236,11,251,15,252,20,248,165,251,100,255,0,194,47,241,55,224,255,0,236,129,251,52,252,46,248,139,225,175,248,103,159,218,175,91,255,0,132,119,199,127,15,254,12,120,43,194,126,46,208,191,182,124,59,240,54,239,79,213,190,199,226,13,35,80,183,251,85,141,221,213,157,199,217,252,219,91,137,160,116,145,190,192,255,0,136,163,191,224,133,31,244,124,223,249,172,223,182,31,255,0,67,237,0,126,255,0,127,159,243,154,254,88,126,46,248,251,197,95,22,60,125,255,0,5,167,240,7,128,255,0,105,127,217,130,251,246,47,241,79,237,189,240,199,246,122,255,0,130,134,248,95,195,190,1,212,190,37,126,211,255,0,4,126,1,124,114,255,0,130,121,254,199,159,178,127,198,223,218,15,194,63,21,116,111,218,91,77,240,167,194,127,15,248,79,197,62,26,241,133,135,136,181,31,27,248,38,239,69,248,121,167,252,9,248,171,227,109,90,235,197,119,62,2,186,248,109,127,245,7,252,69,27,255,0,4,40,255,0,163,229,255,0,205,102,253,176,255,0,250,31,104,255,0,136,163,127,224,133,31,244,124,191,249,172,223,182,31,255,0,67,237,0,111,252,87,212,126,1,124,84,255,0,130,150,252,30,248,19,162,254,211,63,7,245,127,130,223,240,81,175,217,131,69,253,168,62,38,254,207,158,26,241,135,130,239,124,63,251,84,120,87,246,90,210,188,101,225,79,11,165,206,159,160,248,197,37,248,161,225,255,0,141,126,8,253,160,126,22,106,122,182,181,105,4,214,222,48,248,95,255,0,4,152,189,248,109,226,8,60,117,240,211,89,215,244,255,0,8,252,129,251,83,254,213,31,0,180,191,10,255,0,193,86,237,52,63,218,191,246,96,240,15,141,180,79,248,46,247,252,18,102,218,203,196,63,16,252,127,224,191,17,248,87,192,62,42,240,118,155,255,0,4,135,139,84,214,126,32,248,58,211,226,127,135,110,245,111,15,248,119,87,253,152,127,104,201,252,67,166,38,187,160,220,139,127,217,195,199,214,178,106,218,68,158,23,215,111,52,159,168,63,226,40,223,248,33,71,253,31,47,254,107,55,237,135,255,0,208,251,71,252,69,29,255,0,4,40,255,0,163,230,255,0,205,102,253,176,255,0,250,31,104,3,231,248,127,110,31,217,99,194,158,44,255,0,130,117,248,131,227,183,237,241,251,0,124,74,248,154,223,240,83,255,0,218,131,227,31,199,95,218,123,224,111,136,254,29,124,46,253,156,124,109,225,205,79,246,30,255,0,130,145,124,43,248,53,115,117,227,249,117,169,52,43,207,136,62,15,248,11,173,254,199,30,5,241,54,143,121,226,223,22,248,171,193,209,120,147,225,174,155,226,175,16,235,16,248,187,192,254,34,241,103,212,30,62,255,0,130,151,255,0,193,61,39,255,0,130,145,254,202,30,62,184,253,182,63,101,253,23,225,230,141,251,16,127,193,65,188,50,62,37,120,167,227,87,128,124,31,240,207,196,122,143,140,190,60,255,0,193,50,111,252,43,115,240,255,0,226,63,138,181,187,61,19,226,111,135,245,11,159,135,95,21,244,187,93,91,195,183,250,166,149,47,136,126,10,120,243,195,11,123,255,0,9,23,129,124,95,165,232,188,255,0,252,69,29,255,0,4,40,255,0,163,230,255,0,205,102,253,176,255,0,250,31,104,255,0,136,163,127,224,133,31,244,124,191,249,172,223,182,31,255,0,67,237,0,125,255,0,251,5,127,196,226,235,246,201,248,151,224,255,0,222,126,206,95,26,191,107,253,83,226,207,236,167,169,233,159,232,126,4,241,103,194,207,17,126,206,63,179,110,159,241,55,226,95,194,221,8,121,105,101,240,255,0,198,31,181,214,137,251,76,248,186,223,89,182,181,131,78,248,129,121,227,155,239,138,58,53,206,191,163,120,231,79,241,78,187,247,253,126,0,255,0,196,81,223,240,66,143,250,62,111,252,214,111,219,15,255,0,161,246,143,248,138,59,254,8,81,255,0,71,205,255,0,154,205,251,97,255,0,244,62,208,7,239,245,126,0,255,0,193,209,223,242,130,143,219,155,254,237,155,255,0,91,15,246,125,163,254,34,142,255,0,130,20,127,209,243,127,230,179,126,216,127,253,15,181,249,3,255,0,5,234,255,0,130,245,127,193,39,127,109,47,248,36,247,237,89,251,52,126,205,31,181,103,252,44,159,141,191,18,191,225,70,127,194,21,224,175,248,81,159,180,151,131,191,182,191,225,14,253,164,190,15,120,255,0,196,127,241,81,248,255,0,224,246,149,164,105,223,102,240,151,133,117,235,191,244,187,248,60,239,176,121,16,121,183,50,195,12,128,31,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 3218; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

