#ifndef GETVESUITE_Filter_Filter_ROTARY_H
#define GETVESUITE_Filter_Filter_ROTARY_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Filter_Filter_ROTARY( void )
{
    unsigned char osgData[ 2713 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,45,0,51,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,254,43,226,223,219,223,246,220,248,119,251,2,126,207,218,143,198,255,0,30,232,154,231,139,245,29,71,196,26,127,195,255,0,134,63,15,252,61,13,202,106,31,16,190,40,248,131,76,214,181,95,15,120,90,109,114,61,62,230,15,8,232,191,217,222,30,215,47,245,45,90,234,41,86,207,78,208,238,154,206,207,85,213,95,79,209,245,13,63,248,39,183,197,47,29,252,113,253,129,127,97,239,141,127,20,181,223,248,74,62,38,252,96,253,144,63,102,159,138,95,17,124,77,253,153,163,104,159,240,145,120,239,226,7,193,143,5,120,179,197,218,239,246,55,135,116,251,77,63,72,251,95,136,53,125,66,227,236,182,54,150,214,118,255,0,104,242,173,109,225,129,18,53,254,96,63,224,177,222,53,95,143,95,240,85,45,51,225,141,165,247,137,95,66,253,153,190,16,248,3,192,183,254,30,241,30,169,123,109,225,91,111,136,191,17,133,223,197,207,17,120,175,193,58,78,157,170,92,66,177,234,63,14,252,95,240,199,77,212,175,229,183,177,190,185,187,240,82,90,203,12,214,154,102,157,117,38,24,138,202,133,25,212,234,182,245,28,85,221,143,155,190,33,252,116,255,0,130,133,254,222,222,37,213,60,91,241,99,227,119,143,62,29,120,83,198,126,21,210,60,11,171,252,5,248,31,226,207,136,127,12,254,3,183,132,46,108,238,108,124,73,161,106,62,9,182,241,93,209,241,226,235,22,218,229,220,154,180,250,237,222,169,115,122,154,195,233,137,58,232,214,150,26,117,135,153,233,223,179,15,199,207,217,202,195,88,186,253,158,126,50,252,103,248,25,55,138,96,70,241,64,248,63,241,39,226,23,195,33,226,201,244,37,184,95,12,167,136,224,240,150,183,102,222,33,75,41,245,189,92,90,9,156,253,144,107,55,126,91,198,46,37,99,251,189,251,39,124,6,210,239,244,221,46,21,181,137,97,134,206,221,35,104,133,160,143,59,86,88,33,17,180,49,136,81,0,142,89,118,194,124,198,1,75,50,128,181,244,223,196,143,131,190,1,241,31,131,117,13,107,193,90,175,135,60,81,165,88,234,190,42,240,213,205,239,134,53,109,35,86,211,34,241,63,129,188,77,173,120,59,199,126,25,187,190,177,189,117,181,241,14,143,227,31,14,107,58,78,167,108,101,23,58,86,175,225,187,221,58,238,27,123,219,75,152,87,227,170,102,213,221,105,55,57,89,105,166,202,239,79,75,157,10,154,105,105,175,159,200,240,223,248,37,167,252,21,227,226,7,199,127,138,182,31,177,239,237,133,161,233,150,31,29,181,125,42,121,254,13,124,84,240,119,133,245,13,23,64,248,199,105,224,239,6,75,174,120,179,76,241,231,135,224,154,123,127,8,124,77,58,63,135,124,65,226,22,190,177,143,78,240,205,252,77,123,167,91,233,158,29,185,178,209,173,188,73,253,10,87,249,245,254,223,31,15,181,111,3,221,199,227,175,3,106,243,120,91,198,30,20,215,45,124,89,225,143,21,104,58,198,171,160,248,139,195,190,39,240,244,182,250,190,135,226,45,15,84,208,224,130,93,51,93,179,150,214,41,237,46,226,100,184,183,188,142,57,225,145,30,40,158,63,233,178,89,255,0,111,255,0,25,254,206,191,6,126,37,252,25,248,241,241,227,227,159,193,47,137,190,21,248,119,241,175,66,241,87,195,111,14,254,194,127,5,127,224,166,186,79,131,188,123,224,99,175,233,158,7,241,102,147,241,231,224,254,179,251,55,126,208,182,247,154,239,196,93,46,230,252,233,182,63,179,245,247,130,124,61,240,154,8,172,245,191,138,250,245,245,228,58,199,212,101,216,183,138,163,205,47,138,38,18,138,141,173,212,253,168,162,188,3,246,92,214,127,225,32,248,19,224,109,95,254,22,63,199,255,0,139,127,107,255,0,132,155,254,46,23,237,71,240,87,254,25,223,227,183,136,60,143,24,248,130,219,254,43,159,131,191,240,160,190,23,255,0,194,31,246,79,39,236,58,103,252,80,186,31,246,134,143,166,233,250,167,252,76,254,219,253,177,168,21,232,18,124,255,0,255,0,4,157,255,0,148,89,127,193,52,255,0,236,192,63,99,127,253,103,95,135,53,252,247,255,0,193,110,62,18,248,131,224,103,252,20,27,192,127,180,230,141,224,77,111,67,248,81,241,255,0,225,231,129,244,15,24,252,80,183,154,61,95,195,186,223,237,11,224,89,124,77,160,201,160,234,173,170,234,179,15,7,235,143,240,71,195,223,15,19,77,179,72,108,44,245,136,252,61,168,221,233,241,94,95,88,120,134,230,15,232,67,254,9,59,255,0,40,178,255,0,130,105,255,0,217,128,126,198,255,0,250,206,191,14,107,220,63,106,239,217,71,224,175,237,161,240,95,196,95,2,254,59,120,107,251,123,194,122,212,144,234,154,62,173,96,214,214,126,46,240,15,140,116,232,110,162,208,62,32,248,3,92,185,179,156,104,30,49,211,141,229,208,134,102,134,123,91,171,91,219,189,43,85,179,212,116,109,67,81,211,174,241,196,81,85,233,78,155,211,155,111,81,166,211,186,63,6,255,0,101,95,218,54,194,195,77,210,102,26,130,47,218,45,109,148,43,220,201,10,171,184,136,91,20,137,98,143,109,194,121,240,171,43,68,90,72,157,93,9,229,107,156,253,155,191,104,141,62,211,246,110,241,254,150,151,171,31,218,255,0,108,175,248,41,46,166,160,75,37,180,187,53,191,248,40,159,237,73,172,254,226,48,235,230,74,233,172,90,170,146,197,81,75,23,10,163,47,241,151,197,95,248,37,103,252,20,183,246,65,241,70,177,99,240,131,194,243,126,215,63,7,60,49,225,77,43,197,177,252,87,240,29,207,131,124,27,227,107,219,109,42,210,246,243,197,62,25,212,62,8,120,131,199,183,58,238,177,227,75,119,211,25,108,236,124,56,117,243,173,218,205,166,253,146,87,213,238,103,209,116,239,135,127,100,223,128,255,0,240,83,127,218,11,225,79,196,11,47,130,63,178,103,197,59,253,63,195,223,29,255,0,106,229,215,117,15,30,255,0,194,53,240,63,74,95,20,120,139,246,166,248,225,38,177,163,233,151,95,25,117,237,9,124,85,173,104,222,49,210,124,75,166,235,150,186,83,92,220,248,123,81,210,141,150,170,182,87,114,219,171,252,203,202,107,37,94,60,154,212,113,215,166,143,250,251,205,149,69,116,239,162,255,0,128,118,191,181,166,169,241,31,246,144,248,133,225,255,0,128,255,0,3,180,29,95,199,127,21,126,38,235,109,225,159,5,248,63,195,214,186,85,197,214,177,168,92,217,61,237,197,196,151,58,130,91,199,166,232,86,90,37,181,245,198,163,170,92,180,90,94,151,167,217,207,170,106,23,182,150,86,247,87,54,255,0,222,255,0,195,191,0,120,75,225,71,195,255,0,2,252,45,240,6,147,253,129,224,79,134,190,14,240,199,128,60,21,161,125,191,83,213,127,177,124,37,224,221,18,199,195,190,27,210,127,181,53,171,219,155,205,75,236,218,54,157,101,15,159,119,113,61,204,222,79,153,60,210,202,206,237,249,27,255,0,4,211,255,0,130,64,120,71,246,46,241,37,239,199,191,140,222,46,210,62,57,254,212,122,214,153,111,103,164,120,142,211,64,151,76,240,39,192,189,51,84,240,245,181,151,139,60,47,240,154,211,83,184,146,231,87,213,46,47,238,181,235,41,252,93,121,14,157,168,94,232,115,37,133,158,141,225,232,117,15,17,91,235,95,180,117,239,224,48,191,85,163,200,254,39,185,156,228,155,211,100,127,144,47,252,29,29,255,0,41,215,253,185,191,238,217,191,245,143,63,103,218,40,255,0,131,163,191,229,58,255,0,183,55,253,219,55,254,177,231,236,251,69,118,144,127,167,223,252,18,119,254,81,101,255,0,4,211,255,0,179,0,253,141,255,0,245,157,126,28,215,223,245,240,7,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,52,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,64,31,127,215,192,31,240,77,63,249,55,95,136,223,246,127,255,0,240,86,47,253,122,111,237,145,71,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,254,119,53,241,7,252,19,231,254,9,149,255,0,4,220,241,167,192,111,30,235,30,49,255,0,130,124,254,196,30,44,213,236,191,109,255,0,248,41,183,132,236,245,79,19,126,202,31,1,181,237,70,211,194,190,2,255,0,130,146,126,213,254,5,240,47,134,109,175,181,79,0,203,44,30,31,209,124,19,225,207,15,104,250,77,146,176,182,211,180,173,10,207,79,179,142,27,75,88,33,64,15,221,234,43,224,15,248,116,239,252,18,203,254,145,167,251,0,127,226,27,254,206,191,252,238,104,255,0,135,78,255,0,193,44,191,233,26,127,176,7,254,33,191,236,235,255,0,206,230,128,63,204,19,254,14,142,255,0,148,235,254,220,223,247,108,223,250,199,159,179,237,21,254,159,127,240,233,223,248,37,151,253,35,79,246,0,255,0,196,55,253,157,127,249,220,209,64,31,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 2713; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

