#ifndef GETVESUITE_Filter_Filter_BLOCK_H
#define GETVESUITE_Filter_Filter_BLOCK_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Filter_Filter_BLOCK( void )
{
    unsigned char osgData[ 2386 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,61,0,61,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,134,127,224,153,63,240,76,175,248,38,231,143,191,224,155,159,240,79,159,28,248,235,254,9,243,251,15,248,211,198,254,52,253,136,63,101,15,22,120,199,198,62,44,253,148,62,3,120,143,197,94,44,241,79,136,254,3,248,11,88,241,7,137,188,77,226,29,99,192,83,93,235,190,32,191,213,175,47,46,175,111,46,166,150,226,234,230,234,73,231,149,229,119,99,247,7,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,52,127,193,39,127,229,22,95,240,77,63,251,48,15,216,223,255,0,89,215,225,205,125,255,0,64,31,0,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,71,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,53,247,253,20,1,240,7,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,52,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,95,127,209,64,31,0,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,113,95,196,31,252,30,79,251,39,126,203,63,178,247,252,59,147,254,25,159,246,106,248,1,251,59,255,0,194,112,127,107,239,248,77,127,225,70,124,28,248,117,240,147,254,19,15,248,70,71,236,189,255,0,8,223,252,37,31,240,128,248,119,79,255,0,132,128,105,255,0,240,144,107,223,97,251,95,157,246,79,237,187,207,35,203,251,76,219,255,0,209,238,191,128,63,248,62,115,254,113,117,255,0,119,179,255,0,190,143,64,31,215,239,252,18,119,254,81,101,255,0,4,211,255,0,179,0,253,141,255,0,245,157,126,28,215,223,245,240,7,252,18,119,254,81,101,255,0,4,211,255,0,179,0,253,141,255,0,245,157,126,28,215,223,244,0,81,69,20,0,81,69,20,0,87,240,7,255,0,7,206,127,206,46,191,238,246,127,247,209,235,251,252,175,224,15,254,15,156,255,0,156,93,127,221,236,255,0,239,163,208,7,245,251,255,0,4,157,255,0,148,89,127,193,52,255,0,236,192,63,99,127,253,103,95,135,53,247,253,124,1,255,0,4,157,255,0,148,89,127,193,52,255,0,236,192,63,99,127,253,103,95,135,53,247,253,0,20,81,69,0,20,81,69,0,21,252,1,255,0,193,243,159,243,139,175,251,189,159,253,244,122,254,255,0,43,248,3,255,0,131,231,63,231,23,95,247,123,63,251,232,244,1,253,126,255,0,193,39,127,229,22,95,240,77,63,251,48,15,216,223,255,0,89,215,225,205,116,31,240,82,239,28,252,76,248,87,255,0,4,244,253,182,62,47,124,25,248,147,226,15,132,127,21,190,12,126,204,31,26,190,54,124,63,241,239,134,116,63,135,190,37,212,116,175,20,252,29,240,14,185,241,59,73,176,185,208,126,41,248,43,196,58,38,169,225,253,82,231,194,169,165,106,209,92,233,82,92,157,43,91,189,58,101,222,151,170,139,45,82,203,248,99,253,147,255,0,224,242,143,248,101,239,217,103,246,105,253,154,127,225,220,159,240,156,255,0,195,60,124,0,248,55,240,51,254,19,95,248,107,239,248,70,191,225,48,255,0,133,75,240,231,195,126,2,255,0,132,167,254,17,207,248,101,253,67,254,17,255,0,237,15,236,15,181,253,139,237,247,223,100,251,95,145,246,187,157,158,115,250,7,197,47,248,61,11,225,103,199,47,2,107,191,11,126,53,255,0,193,28,62,31,252,96,248,101,226,127,236,179,226,111,135,95,20,191,106,111,14,252,64,240,39,136,191,177,117,125,63,196,154,63,246,231,132,124,89,251,27,93,233,250,183,217,60,65,164,105,87,214,223,104,183,147,200,188,211,45,238,162,219,60,49,186,128,126,175,124,83,248,203,255,0,5,65,248,123,240,207,226,255,0,196,207,18,254,220,63,182,255,0,193,61,91,75,253,136,63,224,179,127,181,135,195,239,129,223,27,126,4,127,193,51,159,226,111,129,53,31,248,38,231,196,47,217,171,225,39,192,221,39,226,175,136,60,23,251,24,79,162,124,66,240,255,0,141,238,124,119,241,19,199,26,141,231,133,100,182,210,167,240,247,143,188,39,163,248,83,196,90,181,158,135,63,143,60,111,250,189,251,121,106,63,180,7,135,190,29,124,24,189,253,159,63,224,161,159,16,60,11,123,240,111,246,128,253,144,127,100,95,218,62,63,6,124,60,253,144,60,123,241,79,226,63,142,255,0,106,207,218,159,246,17,248,73,31,140,62,44,106,94,55,248,29,173,232,95,9,190,32,105,63,1,126,49,248,239,197,22,218,14,131,224,223,15,217,234,122,143,198,239,12,235,255,0,99,182,240,198,151,103,160,235,95,198,23,143,255,0,224,233,47,216,23,226,191,252,33,95,240,180,191,224,221,175,217,3,226,87,252,43,95,135,254,28,248,77,240,231,254,19,255,0,136,127,6,60,99,255,0,8,15,194,191,7,125,179,254,17,31,134,158,10,255,0,132,139,246,19,185,255,0,132,91,192,26,87,246,142,163,253,155,163,88,249,26,117,143,219,230,251,53,180,94,107,238,81,255,0,7,73,126,192,195,225,96,248,22,63,224,221,191,217,3,254,20,145,248,129,255,0,11,103,254,20,239,252,44,63,130,255,0,240,171,63,225,105,255,0,194,59,255,0,8,113,248,151,255,0,10,251,254,24,75,251,39,254,22,7,252,34,67,251,47,251,103,236,159,218,63,217,223,232,63,105,251,47,238,168,3,250,60,248,101,174,255,0,193,70,117,31,218,199,227,167,236,159,241,83,254,10,95,251,95,248,91,194,95,3,252,127,240,179,193,23,127,183,84,223,2,63,224,151,158,19,253,156,124,83,227,189,75,225,183,252,18,199,196,154,135,236,223,105,240,255,0,85,253,148,53,29,95,195,63,31,254,35,248,183,246,215,248,189,105,224,37,213,124,69,109,103,165,197,107,224,251,61,26,207,227,22,173,165,248,218,210,219,236,31,248,34,47,237,71,251,76,126,211,127,4,206,187,251,73,248,235,226,7,138,252,65,115,240,3,246,55,248,176,240,124,123,240,207,236,247,224,159,141,154,143,140,126,63,124,34,212,190,34,248,211,226,95,195,79,14,254,203,254,30,210,252,51,169,254,192,30,32,177,151,193,214,255,0,10,53,157,78,214,79,29,159,23,248,39,227,23,133,188,107,115,22,161,225,24,52,141,35,248,227,241,111,252,28,201,255,0,4,220,241,247,138,60,77,227,191,28,255,0,193,182,223,177,7,141,60,109,227,77,127,89,241,111,140,124,97,226,207,20,252,6,241,31,138,124,89,226,175,17,106,119,26,183,136,124,75,226,95,16,235,31,176,84,215,122,247,136,47,245,123,219,187,187,219,219,185,101,185,186,185,186,150,105,229,121,36,102,62,189,240,183,254,14,235,253,150,126,7,107,58,31,136,190,10,127,193,9,126,0,124,31,241,7,133,254,31,234,127,9,252,53,174,252,46,248,235,240,235,225,254,179,225,223,133,154,223,142,245,31,138,90,207,195,77,11,83,240,159,236,73,105,62,147,240,254,239,226,110,175,170,248,142,235,70,183,146,61,58,125,119,83,184,213,229,182,125,66,105,46,24,3,253,31,43,248,3,255,0,131,231,63,231,23,95,247,123,63,251,232,244,127,196,115,159,245,139,175,252,221,159,255,0,36,122,252,1,255,0,130,231,127,193,115,191,225,244,39,246,94,3,246,93,255,0,134,108,255,0,134,108,63,27,71,63,27,7,198,63,248,76,255,0,225,113,127,194,163,227,254,73,23,133,127,225,28,26,119,252,42,207,250,127,251,95,246,247,252,186,253,151,253,32,3,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 2386; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

