#ifndef GETVESUITE_RCSTR_RCSTR_BLOCK_H
#define GETVESUITE_RCSTR_RCSTR_BLOCK_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_RCSTR_RCSTR_BLOCK( void )
{
    unsigned char osgData[ 3131 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,95,0,95,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,134,191,224,153,63,240,76,159,248,38,223,143,127,224,155,127,240,79,159,29,120,235,254,9,243,251,16,120,211,198,222,52,253,136,63,101,15,22,120,199,198,62,44,253,148,62,3,120,143,197,94,44,241,87,136,254,3,120,7,88,241,15,137,188,77,226,29,99,192,51,93,235,222,32,191,213,239,47,46,175,111,110,166,150,230,234,230,234,73,231,145,229,118,99,246,255,0,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,52,127,193,39,127,229,22,95,240,77,63,251,48,15,216,223,255,0,89,215,225,205,125,255,0,64,31,0,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,71,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,53,247,253,20,1,240,7,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,52,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,95,127,209,64,31,0,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,71,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,53,247,253,20,1,240,7,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,52,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,95,127,209,64,31,0,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,95,136,63,240,113,231,252,19,219,246,5,248,29,255,0,4,99,253,178,126,41,124,20,253,135,191,100,15,131,255,0,19,124,47,255,0,12,243,255,0,8,207,196,95,133,191,179,79,193,127,135,254,59,240,239,246,223,237,87,240,55,195,186,207,246,23,139,188,39,224,171,77,67,72,251,95,135,245,125,86,198,235,236,247,17,253,162,207,83,184,181,151,124,19,72,141,253,94,215,224,15,252,29,29,255,0,40,40,253,185,191,238,217,191,245,176,255,0,103,218,0,251,255,0,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,248,3,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,0,40,162,138,0,40,162,138,0,40,162,138,0,40,162,138,0,43,240,7,254,14,142,255,0,148,20,126,220,223,247,108,223,250,216,127,179,237,126,255,0,87,224,15,252,29,29,255,0,40,40,253,185,191,238,217,191,245,176,255,0,103,218,0,251,255,0,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,248,3,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,0,40,162,138,0,40,162,138,0,40,162,138,0,40,162,138,0,43,240,7,254,14,142,255,0,148,20,126,220,223,247,108,223,250,216,127,179,237,126,255,0,87,224,15,252,29,29,255,0,40,40,253,185,191,238,217,191,245,176,255,0,103,218,0,251,255,0,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,248,3,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,0,40,162,138,0,40,162,138,0,40,162,138,0,40,162,138,0,43,240,7,254,14,142,255,0,148,20,126,220,223,247,108,223,250,216,127,179,237,126,255,0,87,224,15,252,29,29,255,0,40,40,253,185,191,238,217,191,245,176,255,0,103,218,0,251,255,0,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,248,3,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,239,250,0,40,162,190,95,248,215,251,110,254,197,255,0,179,95,138,180,255,0,2,254,209,127,181,223,236,193,240,7,198,218,175,135,237,124,91,165,248,59,227,95,199,223,133,63,10,252,83,169,120,86,251,81,213,116,123,31,18,216,120,127,199,94,44,176,187,188,240,252,218,190,131,174,90,197,121,28,45,109,37,198,141,119,2,200,101,183,153,80,3,234,10,43,231,255,0,31,126,214,63,178,207,194,143,133,158,10,248,231,241,75,246,150,253,159,254,27,124,18,248,147,255,0,8,231,252,43,175,140,94,63,248,201,240,235,193,223,11,60,127,255,0,9,143,135,111,124,97,225,15,248,66,190,33,120,139,196,150,218,71,138,191,181,124,37,167,106,26,166,155,246,27,185,254,221,167,88,77,125,107,230,218,196,242,170,120,139,246,177,253,150,124,33,172,124,58,240,239,139,63,105,111,128,30,23,241,7,198,15,136,30,46,248,77,240,151,66,241,23,198,79,135,90,46,179,241,75,226,159,195,255,0,29,193,240,183,199,159,13,126,29,105,154,151,136,226,155,198,223,16,52,79,137,183,54,254,28,213,244,109,50,59,173,71,77,215,110,35,210,47,45,161,212,29,109,200,7,208,52,87,195,254,38,255,0,130,155,127,193,55,60,23,168,219,104,254,49,255,0,130,131,126,196,30,19,213,239,60,63,225,63,22,90,105,126,38,253,171,254,3,104,90,141,215,133,124,123,225,109,27,199,94,5,241,45,181,150,169,227,216,165,159,195,218,215,130,124,69,225,237,99,73,189,84,54,218,142,149,175,89,106,54,114,205,105,117,4,210,30,38,255,0,130,155,127,193,55,60,23,168,219,104,254,49,255,0,130,131,254,196,30,18,213,239,60,63,225,47,22,218,105,94,38,253,171,254,3,104,90,141,215,133,124,123,225,93,27,199,94,6,241,45,181,150,169,227,232,165,159,195,250,215,130,60,71,225,237,99,73,189,85,54,218,142,149,174,217,106,54,114,77,105,117,12,210,0,125,193,69,20,80,1,95,128,63,240,116,119,252,160,163,246,230,255,0,187,102,255,0,214,195,253,159,107,247,250,191,0,127,224,232,239,249,65,71,237,205,255,0,118,205,255,0,173,135,251,62,208,7,223,255,0,240,73,223,249,69,151,252,19,79,254,204,3,246,55,255,0,214,117,248,115,95,127,215,242,133,255,0,4,246,255,0,131,143,63,224,140,127,3,191,96,95,216,123,224,167,197,47,219,39,254,17,127,137,191,7,255,0,100,15,217,167,225,111,196,95,13,127,195,60,254,213,122,215,252,35,190,59,248,127,240,99,193,94,19,241,118,133,253,179,225,223,129,183,122,126,173,246,79,16,105,26,133,191,218,172,110,238,172,238,62,207,230,218,220,77,3,164,141,245,255,0,252,69,29,255,0,4,40,255,0,163,230,255,0,205,102,253,176,255,0,250,31,104,3,247,250,191,16,127,110,239,136,255,0,20,226,255,0,130,154,254,199,63,11,127,100,255,0,143,191,178,7,195,191,218,234,243,246,0,255,0,130,143,106,94,9,248,115,251,83,232,222,35,241,238,143,241,3,251,123,226,127,236,27,226,47,3,120,53,252,35,240,219,227,87,132,124,79,225,15,237,251,223,130,63,16,239,160,241,45,165,191,138,62,205,161,124,21,241,197,213,159,132,181,215,209,175,164,210,252,255,0,254,34,142,255,0,130,20,127,209,243,127,230,179,126,216,127,253,15,180,127,196,81,223,240,66,143,250,62,111,252,214,111,219,15,255,0,161,246,128,60,255,0,246,28,248,249,251,44,124,6,248,147,173,254,208,31,27,191,104,175,0,120,23,225,151,196,31,128,26,14,157,167,252,70,253,161,252,77,240,235,225,30,143,240,11,246,250,248,239,251,88,254,215,223,182,207,252,20,219,246,62,241,78,143,169,252,70,215,116,255,0,216,195,246,128,143,196,31,180,15,236,178,250,183,194,63,17,248,174,227,198,122,174,133,240,83,69,178,159,85,248,137,63,193,255,0,16,248,139,69,231,245,253,83,254,9,233,240,43,196,30,33,248,73,240,183,197,90,7,252,18,158,219,197,191,179,6,139,172,120,147,225,239,237,217,240,35,192,51,255,0,193,61,63,104,31,217,231,246,139,253,168,63,111,29,99,225,223,193,175,141,255,0,11,126,42,248,255,0,66,187,248,107,226,11,141,95,196,63,182,39,142,124,31,240,143,78,241,231,193,111,22,195,160,124,101,93,59,197,62,17,215,244,79,134,222,42,248,97,240,255,0,216,63,226,40,239,248,33,71,253,31,55,254,107,55,237,135,255,0,208,251,71,252,69,29,255,0,4,40,255,0,163,230,255,0,205,102,253,176,255,0,250,31,104,0,62,34,253,156,126,6,232,95,240,111,110,145,39,195,175,135,255,0,176,191,136,62,48,124,127,248,113,226,40,63,102,191,136,62,46,181,209,126,42,120,119,89,180,255,0,130,54,254,210,191,179,119,135,126,29,120,195,86,248,139,6,149,226,127,138,254,63,240,165,239,138,127,102,255,0,132,242,234,250,221,163,107,215,26,141,199,131,188,59,120,144,234,23,250,70,154,222,127,241,191,226,135,197,79,130,255,0,183,215,237,159,224,175,248,39,71,142,255,0,100,13,43,197,191,178,255,0,252,19,7,254,9,241,115,226,159,216,218,79,128,158,34,248,239,241,79,84,248,87,240,95,227,63,237,209,226,13,111,225,239,236,225,240,91,224,95,237,51,240,242,231,194,255,0,16,60,49,240,79,226,175,129,230,240,255,0,131,174,52,233,244,239,17,234,63,24,254,24,232,19,107,30,1,210,60,71,103,226,25,253,3,254,34,141,255,0,130,20,127,209,242,255,0,230,179,126,216,127,253,15,180,127,196,81,223,240,66,143,250,62,111,252,214,111,219,15,255,0,161,246,128,63,87,255,0,98,157,55,224,23,135,63,100,127,217,203,192,191,178,215,197,191,15,252,118,253,159,62,20,252,31,240,63,193,79,133,63,22,188,51,227,223,5,252,79,211,188,105,225,95,130,58,37,175,194,59,125,66,231,199,159,14,227,77,19,196,126,32,138,231,193,119,86,186,180,186,108,54,214,201,170,216,94,192,182,150,141,19,91,69,244,253,126,0,255,0,196,81,191,240,66,143,250,62,95,252,214,111,219,15,255,0,161,246,143,248,138,59,254,8,81,255,0,71,205,255,0,154,205,251,97,255,0,244,62,208,7,239,245,126,0,255,0,193,209,223,242,130,143,219,155,254,237,155,255,0,91,15,246,125,163,254,34,142,255,0,130,20,127,209,243,127,230,179,126,216,127,253,15,181,249,1,255,0,5,234,255,0,130,245,127,193,39,191,109,47,248,36,247,237,89,251,52,126,205,31,181,111,252,44,159,141,159,18,127,225,70,127,194,21,224,175,248,81,159,180,151,131,191,182,191,225,14,253,164,190,14,248,251,196,127,241,81,248,255,0,224,238,149,164,233,223,102,240,151,133,117,235,191,244,187,248,60,239,176,125,158,15,54,230,88,97,144,3,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 3131; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

