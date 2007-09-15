#ifndef GETVESUITE_SCFrac_SCFrac_BLOCK_H
#define GETVESUITE_SCFrac_SCFrac_BLOCK_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_SCFrac_SCFrac_BLOCK( void )
{
    unsigned char osgData[ 3284 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,95,0,95,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,134,191,224,153,63,240,76,159,248,38,223,143,127,224,155,127,240,79,159,29,120,235,254,9,243,251,16,120,211,198,222,52,253,136,63,101,15,22,120,199,198,62,44,253,148,62,3,120,143,197,94,44,241,87,136,254,3,120,7,88,241,15,137,188,77,226,29,99,192,51,93,235,222,32,191,213,239,47,46,175,111,110,166,150,230,234,230,234,73,231,145,229,118,99,246,255,0,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,52,127,193,39,127,229,22,95,240,77,63,251,48,15,216,223,255,0,89,215,225,205,125,255,0,64,31,0,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,71,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,53,247,253,20,1,240,7,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,52,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,95,127,209,64,31,0,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,71,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,53,247,253,20,1,240,7,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,52,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,95,127,209,64,31,0,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,95,136,63,240,113,223,252,19,219,246,6,248,29,255,0,4,99,253,178,62,40,252,20,253,135,191,100,15,131,255,0,19,124,47,255,0,12,243,255,0,8,207,196,95,133,191,179,79,193,143,135,254,60,240,233,214,255,0,106,191,129,190,29,214,191,176,188,93,225,47,5,90,106,26,71,218,252,61,171,106,182,55,95,103,184,143,237,22,122,157,197,172,187,160,154,68,111,234,246,191,0,127,224,232,239,249,65,71,237,205,255,0,118,205,255,0,173,135,251,62,208,7,223,255,0,240,73,223,249,69,151,252,19,79,254,204,3,246,55,255,0,214,117,248,115,95,127,215,192,31,240,73,223,249,69,151,252,19,79,254,204,3,246,55,255,0,214,117,248,115,95,127,208,1,69,20,80,1,69,20,80,1,69,20,80,1,69,20,80,1,95,128,63,240,116,119,252,160,163,246,230,255,0,187,102,255,0,214,195,253,159,107,247,250,191,0,127,224,232,239,249,65,71,237,205,255,0,118,205,255,0,173,135,251,62,208,7,223,255,0,240,73,223,249,69,151,252,19,79,254,204,3,246,55,255,0,214,117,248,115,95,127,215,192,31,240,73,223,249,69,151,252,19,79,254,204,3,246,55,255,0,214,117,248,115,95,127,208,1,69,20,80,1,69,20,80,1,69,20,80,1,69,20,80,1,95,128,63,240,116,119,252,160,163,246,230,255,0,187,102,255,0,214,195,253,159,107,247,250,191,0,127,224,232,239,249,65,71,237,205,255,0,118,205,255,0,173,135,251,62,208,7,223,255,0,240,73,223,249,69,151,252,19,79,254,204,3,246,55,255,0,214,117,248,115,95,127,215,192,31,240,73,223,249,69,151,252,19,79,254,204,3,246,55,255,0,214,117,248,115,95,127,208,1,69,20,80,1,69,20,80,1,69,20,80,1,69,20,80,1,95,128,63,240,116,119,252,160,163,246,230,255,0,187,102,255,0,214,195,253,159,107,247,250,191,0,127,224,232,239,249,65,71,237,205,255,0,118,205,255,0,173,135,251,62,208,7,223,255,0,240,73,223,249,69,151,252,19,79,254,204,3,246,55,255,0,214,117,248,115,95,127,215,192,31,240,73,223,249,69,151,252,19,79,254,204,3,246,55,255,0,214,117,248,115,95,127,208,1,69,39,227,235,142,190,159,173,120,7,193,223,218,199,246,88,253,162,62,203,255,0,12,255,0,251,75,124,0,248,231,246,223,248,76,62,199,255,0,10,119,227,39,195,175,137,191,107,255,0,133,121,255,0,10,251,254,19,255,0,178,255,0,194,21,226,59,223,180,127,97,255,0,194,219,248,85,253,177,183,63,217,159,240,179,124,61,246,223,35,251,107,77,251,72,7,208,20,87,199,250,127,252,20,39,246,6,213,188,9,169,252,82,210,191,110,31,216,255,0,84,248,101,162,127,110,255,0,108,252,69,211,191,105,111,130,247,190,4,210,127,225,23,214,126,22,248,119,196,223,218,126,46,183,241,179,105,246,31,217,222,32,248,227,240,82,194,251,205,184,79,178,94,124,96,240,181,181,199,151,63,136,52,152,238,249,251,111,248,41,175,252,19,110,243,194,186,207,142,172,255,0,224,160,223,177,5,223,130,124,59,226,15,12,248,79,196,62,49,182,253,171,254,3,207,225,93,7,197,94,52,211,188,89,172,120,59,195,90,207,136,98,241,233,180,210,252,65,171,105,30,1,241,213,214,153,101,60,209,220,95,219,248,47,86,158,214,57,99,211,175,30,16,15,184,40,175,30,248,41,251,66,252,3,253,165,60,43,168,120,235,246,115,248,227,240,127,227,247,130,116,175,16,93,248,79,84,241,143,193,79,137,126,11,248,169,225,93,55,197,86,58,118,149,172,223,120,106,255,0,196,62,6,214,239,237,44,252,65,14,145,174,232,151,82,217,73,50,220,199,109,172,218,78,241,136,174,33,103,244,15,13,120,179,194,222,52,211,174,117,143,7,120,155,195,254,44,210,108,252,65,226,223,9,221,234,158,25,214,116,237,123,77,181,241,87,128,124,83,172,248,23,199,94,26,185,189,210,174,101,142,15,16,104,190,54,240,231,136,116,125,90,201,156,92,105,218,174,133,121,167,94,71,13,229,172,241,32,7,67,69,124,255,0,225,31,218,199,246,88,248,131,227,191,17,124,45,240,31,237,45,251,63,248,223,226,111,132,62,32,79,240,159,197,159,14,188,35,241,147,225,215,137,124,119,225,127,138,150,186,63,196,95,17,92,252,52,241,23,132,116,111,17,205,168,104,159,16,35,240,255,0,193,255,0,139,119,207,163,92,219,197,168,165,159,194,239,17,221,53,176,131,68,212,222,215,232,10,0,43,240,7,254,14,142,255,0,148,20,126,220,223,247,108,223,250,216,127,179,237,126,255,0,87,224,15,252,29,29,255,0,40,40,253,185,191,238,217,191,245,176,255,0,103,218,0,251,255,0,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,254,80,191,224,158,223,240,113,231,252,17,143,224,119,236,11,251,15,124,20,248,165,251,100,255,0,194,47,241,55,224,255,0,236,129,251,52,252,45,248,139,225,175,248,103,159,218,175,90,255,0,132,119,199,127,15,254,12,120,43,194,126,46,208,191,182,124,59,240,54,239,79,213,190,201,226,13,35,80,183,251,85,141,221,213,157,199,217,252,219,91,137,160,116,145,190,191,255,0,136,163,191,224,133,31,244,124,223,249,172,223,182,31,255,0,67,237,0,126,175,126,219,186,215,193,175,14,126,197,255,0,181,223,136,127,104,207,9,248,131,199,191,179,230,131,251,48,124,125,214,126,59,248,23,194,119,51,217,248,171,198,159,6,180,207,133,62,44,189,248,159,225,63,13,94,90,248,151,69,150,215,196,26,151,130,96,215,44,236,165,143,89,210,100,75,155,200,217,53,59,6,2,234,47,192,31,138,18,120,91,227,215,197,27,201,62,27,248,243,195,255,0,240,81,223,20,248,83,225,7,195,77,3,227,223,198,207,14,252,37,211,124,55,251,109,124,12,253,143,124,23,255,0,5,21,253,132,126,42,126,215,255,0,176,79,252,20,67,224,231,193,13,51,73,211,126,45,248,131,226,39,193,207,248,105,67,225,175,132,90,135,193,239,135,62,58,180,208,255,0,102,255,0,137,31,12,237,252,11,241,19,197,126,47,241,211,207,245,7,252,69,29,255,0,4,40,255,0,163,230,255,0,205,102,253,176,255,0,250,31,104,255,0,136,163,191,224,133,31,244,124,223,249,172,223,182,31,255,0,67,237,0,115,255,0,240,82,95,142,95,240,79,79,141,223,178,63,252,20,107,246,165,248,65,175,252,32,248,179,227,127,7,127,193,40,127,108,223,217,159,86,253,184,126,24,248,239,192,58,231,193,173,11,77,248,213,162,105,103,225,247,236,123,226,15,140,154,47,141,211,74,248,143,241,127,196,159,21,162,240,214,165,225,191,8,216,197,226,29,87,194,114,106,33,175,207,132,37,248,173,225,91,127,136,124,255,0,237,151,226,111,29,252,50,253,139,60,107,251,67,254,210,191,182,23,236,1,172,248,183,246,133,253,191,255,0,224,146,122,135,236,161,251,91,120,71,225,198,143,240,203,224,157,239,236,227,224,79,218,115,246,49,248,199,240,243,75,241,14,131,172,124,113,159,87,248,155,240,255,0,195,62,45,178,253,176,62,40,189,138,124,95,215,98,159,194,30,41,241,23,136,244,175,20,248,87,73,189,187,211,252,49,232,31,241,20,119,252,16,163,254,143,155,255,0,53,155,246,195,255,0,232,125,163,254,34,142,255,0,130,20,127,209,243,127,230,179,126,216,127,253,15,180,1,231,254,49,253,183,191,225,136,181,159,219,155,226,95,237,147,251,73,254,199,254,17,255,0,130,147,248,155,254,20,31,236,137,240,39,193,190,35,181,255,0,134,106,253,156,124,121,240,179,78,241,223,140,245,47,216,227,246,159,179,248,105,241,83,246,136,215,60,123,227,63,128,26,127,142,191,110,95,29,92,252,119,241,174,143,172,141,59,195,17,124,13,241,247,132,252,45,166,106,247,159,12,175,60,89,227,143,63,240,221,255,0,237,31,251,48,248,59,246,181,255,0,130,90,207,227,239,135,254,23,253,174,191,106,255,0,216,3,227,207,198,159,216,23,193,190,2,253,188,174,190,51,126,208,151,159,180,127,195,255,0,129,48,124,29,248,149,227,141,119,226,191,196,127,131,31,3,39,248,127,227,255,0,138,63,19,98,240,239,197,141,38,239,72,240,181,212,186,247,196,205,3,246,151,248,169,226,63,25,89,187,253,139,75,250,3,254,34,142,255,0,130,20,127,209,243,127,230,179,126,216,127,253,15,180,127,196,81,223,240,66,143,250,62,111,252,214,111,219,15,255,0,161,246,128,62,191,248,121,227,255,0,248,39,23,237,17,241,123,246,75,209,62,0,248,43,225,255,0,197,175,137,191,177,191,252,38,35,225,197,175,193,255,0,13,233,126,20,214,127,224,156,126,28,213,126,10,120,155,224,239,136,60,13,241,235,193,237,121,160,234,31,179,15,246,167,135,239,45,252,11,23,193,255,0,16,233,112,120,166,235,94,208,146,237,62,31,8,62,20,120,159,196,63,15,255,0,79,235,240,7,254,34,142,255,0,130,20,127,209,243,127,230,179,126,216,127,253,15,180,127,196,81,223,240,66,143,250,62,111,252,214,111,219,15,255,0,161,246,128,63,127,171,240,7,254,14,142,255,0,148,20,126,220,223,247,108,223,250,216,127,179,237,31,241,20,119,252,16,163,254,143,155,255,0,53,155,246,195,255,0,232,125,175,200,31,248,47,87,252,23,171,254,9,59,251,104,255,0,193,39,191,106,207,217,163,246,105,253,171,63,225,100,252,108,248,149,255,0,10,55,254,16,175,5,127,194,140,253,164,252,29,253,181,255,0,8,119,237,37,240,119,199,254,36,255,0,138,147,199,223,7,116,189,39,78,251,55,132,188,45,175,93,255,0,165,223,193,231,125,131,236,246,254,109,204,176,195,32,7,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 3284; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

