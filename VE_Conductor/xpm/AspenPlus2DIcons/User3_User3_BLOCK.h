#ifndef GETVESUITE_User3_User3_BLOCK_H
#define GETVESUITE_User3_User3_BLOCK_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_User3_User3_BLOCK( void )
{
    unsigned char osgData[ 2312 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,61,0,61,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,134,191,224,153,63,240,76,159,248,38,223,143,127,224,155,127,240,79,159,29,120,235,254,9,243,251,16,120,211,198,222,52,253,136,63,101,15,22,120,199,198,62,44,253,148,62,3,120,143,197,94,44,241,87,136,254,3,120,7,88,241,15,137,188,77,226,29,99,192,51,93,235,222,32,191,213,239,47,46,175,111,110,166,150,230,234,230,234,73,231,145,229,118,99,246,255,0,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,52,127,193,39,127,229,22,95,240,77,63,251,48,15,216,223,255,0,89,215,225,205,125,255,0,64,31,0,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,71,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,53,247,253,20,1,240,7,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,52,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,95,127,209,64,31,0,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,95,196,31,252,30,83,251,39,126,203,63,178,255,0,252,59,143,254,25,163,246,105,248,1,251,59,255,0,194,113,255,0,13,125,255,0,9,175,252,40,207,131,127,14,190,18,127,194,97,255,0,8,207,252,50,247,252,35,159,240,148,127,194,1,225,205,63,254,18,15,236,255,0,248,72,53,239,176,253,175,206,251,39,246,221,231,145,229,253,166,109,255,0,232,247,95,192,31,252,31,57,255,0,56,186,255,0,187,217,255,0,223,71,160,15,235,247,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,248,3,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,0,40,162,138,0,40,162,138,0,43,248,3,255,0,131,231,63,231,23,95,247,123,63,251,232,245,253,254,87,240,7,255,0,7,206,127,206,46,191,238,246,127,247,209,232,3,250,253,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,190,0,255,0,130,78,255,0,202,44,191,224,154,127,246,96,31,177,191,254,179,175,195,154,251,254,128,10,40,162,128,10,40,162,128,10,254,0,255,0,224,249,207,249,197,215,253,222,207,254,250,61,127,127,149,252,1,255,0,193,243,159,243,139,175,251,189,159,253,244,122,0,254,191,127,224,147,191,242,139,47,248,38,159,253,152,7,236,111,255,0,172,235,240,230,190,222,241,110,143,168,248,143,194,222,37,240,246,143,226,207,16,120,11,87,215,124,63,172,232,218,95,142,188,39,109,225,91,223,21,120,47,81,212,244,235,139,43,47,22,120,106,207,199,94,26,214,180,75,191,16,105,215,51,197,121,101,30,177,163,106,218,83,220,217,196,186,142,153,127,104,102,181,151,252,233,63,100,239,248,60,167,254,25,127,246,88,253,154,127,102,143,248,119,31,252,39,31,240,206,255,0,179,255,0,193,191,129,127,240,154,255,0,195,95,127,194,51,255,0,9,135,252,42,79,135,94,28,240,7,252,37,63,240,142,127,195,47,106,31,240,143,255,0,104,127,194,63,246,191,176,253,190,251,236,159,107,242,62,217,115,229,249,207,244,7,252,71,57,255,0,88,186,255,0,205,217,255,0,242,71,160,15,215,255,0,15,126,208,159,183,182,173,240,139,254,9,95,227,141,47,246,228,248,129,168,248,183,246,164,255,0,130,96,79,251,72,124,92,240,215,139,124,47,251,8,120,30,215,226,143,197,223,21,124,108,255,0,130,121,120,79,251,31,225,38,179,226,143,217,223,69,210,60,61,241,254,231,194,95,183,247,196,15,13,252,28,209,53,93,111,74,240,78,175,241,31,225,207,193,187,31,29,174,167,164,92,124,68,151,198,169,227,47,143,223,182,236,254,63,253,129,60,15,240,175,246,182,253,191,254,49,105,159,28,63,225,173,60,1,241,217,62,27,252,18,255,0,130,106,120,39,226,167,193,175,17,252,19,255,0,130,159,126,204,191,178,199,137,124,107,241,222,199,196,31,178,175,142,180,141,75,254,20,223,132,191,105,175,136,158,13,248,135,169,124,48,180,213,188,43,227,45,71,246,118,208,124,105,225,248,124,49,224,157,71,197,190,56,79,231,138,235,254,14,191,253,139,239,180,239,141,122,61,239,252,27,255,0,251,48,94,105,63,180,167,136,52,175,22,126,209,122,93,215,197,207,133,55,26,119,199,223,21,104,62,42,212,60,117,161,248,155,227,93,148,223,176,217,139,226,167,136,44,188,111,171,106,154,205,165,238,186,183,247,54,250,174,165,113,168,67,34,93,205,36,205,207,248,255,0,254,14,147,253,129,126,43,255,0,194,21,255,0,11,75,254,13,218,253,144,62,37,127,194,182,248,127,225,191,132,223,14,191,225,63,248,135,240,99,198,63,240,128,252,44,240,119,219,127,225,17,248,105,224,175,248,72,191,97,43,159,248,69,126,31,233,95,218,90,135,246,110,141,99,228,105,214,63,111,155,236,182,209,121,175,184,3,250,93,253,143,254,62,254,217,158,39,253,173,60,85,251,52,248,159,246,253,248,193,251,67,252,59,214,181,249,126,24,248,87,246,196,191,248,101,255,0,4,254,240,111,236,235,241,15,195,250,239,252,19,19,246,105,253,170,79,136,63,100,11,175,132,95,8,174,117,43,239,219,125,126,49,252,126,211,188,117,224,175,13,248,206,31,22,124,47,213,126,0,248,95,199,55,160,120,235,196,94,8,213,117,75,95,213,239,248,39,95,197,47,139,223,16,254,22,124,84,240,231,199,205,119,199,247,191,24,62,12,124,127,241,199,194,223,22,120,83,227,46,153,240,76,124,118,248,119,163,92,120,119,192,255,0,21,62,22,104,95,27,188,101,251,47,233,246,31,10,254,34,120,255,0,89,248,47,241,63,225,207,141,5,215,195,173,50,203,66,240,238,143,241,79,72,240,14,167,113,226,63,24,120,63,196,254,47,241,23,240,73,109,255,0,7,51,255,0,193,55,44,252,43,172,248,22,211,254,13,180,253,136,45,124,19,226,63,16,120,103,197,158,33,240,117,183,138,190,3,65,225,93,119,197,94,11,211,188,89,163,248,59,196,186,207,135,162,253,130,133,166,169,226,13,39,72,241,239,142,173,116,203,217,225,123,155,11,111,26,106,208,90,201,20,90,141,226,205,244,255,0,194,223,248,61,19,225,95,192,239,2,104,95,11,126,10,127,193,27,254,31,252,31,248,101,225,127,237,63,248,70,190,29,124,45,253,169,252,59,240,255,0,192,158,29,254,219,214,53,15,17,107,63,216,126,17,240,159,236,109,105,167,233,63,107,241,6,175,170,223,93,125,158,222,63,180,94,106,119,23,82,239,158,105,29,128,63,208,242,191,128,63,248,62,115,254,113,117,255,0,119,179,255,0,190,143,71,252,71,57,255,0,88,186,255,0,205,217,255,0,242,71,175,192,31,248,46,119,252,23,63,254,31,69,255,0,12,187,255,0,24,184,63,102,223,248,102,207,248,93,157,126,54,159,140,95,240,153,255,0,194,226,255,0,133,71,233,240,147,194,191,240,142,127,103,127,194,172,255,0,167,255,0,182,127,110,255,0,203,175,217,127,210,0,63,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 2312; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

