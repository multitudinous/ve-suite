#ifndef GETVESUITE_User2_User2_VALVE4_H
#define GETVESUITE_User2_User2_VALVE4_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_User2_User2_VALVE4( void )
{
    unsigned char osgData[ 2524 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,33,0,35,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,253,221,255,0,130,177,255,0,193,86,62,52,124,15,248,205,167,126,196,159,177,141,175,133,108,62,52,222,248,2,203,198,95,23,254,58,248,167,195,246,191,17,244,239,131,218,87,143,237,188,81,162,120,39,192,255,0,14,60,23,103,175,197,100,127,104,117,142,194,219,197,175,55,140,225,184,208,52,45,30,231,194,211,221,120,75,198,182,30,47,155,251,3,242,171,69,255,0,130,115,248,167,246,202,212,180,159,23,254,214,255,0,17,124,127,251,86,120,205,117,15,16,235,145,201,251,72,120,187,91,248,167,225,143,10,222,120,255,0,91,211,245,79,25,94,252,40,248,109,226,27,147,225,175,130,122,110,161,113,167,233,73,38,149,224,173,11,195,250,71,217,52,27,11,59,109,46,43,29,51,79,183,183,253,36,253,141,191,106,95,248,35,87,252,21,219,197,222,11,241,39,199,15,248,39,223,236,255,0,224,207,219,87,227,191,128,124,55,227,169,60,43,251,87,254,203,63,4,190,36,120,175,226,213,150,139,240,163,193,154,212,151,95,12,255,0,105,27,143,5,234,90,87,198,91,61,59,194,11,119,6,147,164,106,26,142,141,241,7,254,17,143,134,23,250,229,239,128,116,47,15,105,166,104,127,83,191,225,211,255,0,240,75,63,250,70,167,236,3,255,0,136,111,251,58,255,0,243,185,174,28,86,26,182,37,90,158,49,208,143,104,171,254,55,69,69,168,180,220,110,127,57,62,63,255,0,130,26,124,10,248,99,38,139,226,207,4,124,51,240,87,195,127,19,248,110,250,195,197,90,7,143,190,31,104,182,126,0,248,141,224,223,17,232,58,158,155,172,120,99,197,30,4,241,223,131,162,176,213,124,23,226,139,45,82,218,218,234,203,84,210,239,45,239,236,166,130,59,171,89,173,229,138,57,107,157,248,125,255,0,5,36,253,186,63,224,152,254,34,180,180,248,167,227,159,31,254,217,31,178,118,145,171,253,175,197,158,0,248,174,154,191,196,207,218,103,194,190,13,107,223,20,106,126,34,184,248,29,241,247,84,214,98,214,252,107,227,37,190,241,32,212,70,141,241,42,239,197,246,218,165,175,132,180,175,10,120,127,196,31,15,244,201,164,212,236,127,94,127,224,159,63,240,76,191,248,38,239,141,62,3,120,251,88,241,143,252,19,235,246,33,241,102,175,103,251,111,255,0,193,77,124,39,103,170,120,151,246,80,248,15,174,234,54,190,21,240,15,252,20,147,246,175,240,47,129,124,51,109,125,170,120,10,89,96,240,254,139,224,143,14,120,123,70,210,108,213,133,182,157,165,104,86,122,125,156,112,218,90,193,10,122,199,237,57,251,37,127,193,13,63,99,143,134,127,240,183,191,104,223,216,87,254,9,243,240,251,193,51,120,143,70,240,126,147,45,191,236,45,240,139,198,254,42,241,87,139,181,243,117,38,155,225,111,3,124,59,248,121,240,107,86,241,23,143,188,70,116,221,59,89,212,103,177,209,116,171,251,155,77,31,195,154,174,181,119,20,26,70,149,169,94,218,229,134,193,98,48,237,57,99,165,81,45,211,142,159,140,157,134,228,165,180,44,126,178,248,79,197,158,21,241,239,133,124,53,227,175,2,248,151,195,254,52,240,79,141,60,63,163,120,179,193,222,49,240,158,179,167,120,143,194,190,44,240,175,136,244,235,109,99,195,222,37,240,215,136,116,123,153,173,53,223,15,223,233,23,150,151,86,119,182,179,75,109,117,109,117,28,240,72,241,58,177,43,248,131,248,237,255,0,7,143,248,11,246,95,248,153,170,252,3,248,87,255,0,4,179,241,109,151,195,127,134,154,15,128,180,79,4,105,222,55,253,161,124,3,240,75,196,26,71,134,174,62,31,248,95,86,210,180,71,248,85,240,175,225,31,142,252,63,224,173,30,202,215,81,142,211,75,180,211,124,83,169,68,116,187,59,57,164,143,76,158,89,116,155,18,189,36,211,73,167,116,200,62,198,253,145,255,0,224,152,127,7,191,104,223,248,38,239,236,69,47,142,116,111,5,248,139,77,241,191,236,95,251,43,235,58,198,139,226,173,63,72,214,236,46,111,110,254,12,248,11,89,73,238,108,239,221,210,70,75,200,236,36,88,158,28,164,182,49,206,146,37,194,71,34,125,183,240,214,255,0,246,222,255,0,130,121,105,62,44,241,31,136,126,58,104,191,182,31,236,169,225,77,35,85,215,60,75,224,159,218,111,246,139,240,199,133,190,52,124,26,240,159,135,47,181,143,27,248,211,199,63,13,191,106,15,138,246,151,51,124,79,212,95,67,147,197,11,39,134,190,48,120,182,211,70,243,142,133,13,143,196,207,134,94,21,240,237,253,166,171,248,161,255,0,4,209,248,243,251,97,126,215,63,179,95,236,185,240,11,246,38,240,87,252,39,114,124,50,253,154,62,17,252,63,241,111,197,239,24,234,190,52,240,55,236,185,240,179,94,248,101,240,107,192,22,250,255,0,132,62,32,124,111,240,223,129,245,251,88,126,41,164,218,198,137,103,31,132,180,77,63,91,241,146,31,17,105,247,247,218,30,157,225,161,170,120,159,71,254,144,191,102,15,248,36,255,0,129,126,25,248,183,76,248,201,251,87,252,77,186,253,183,254,59,232,122,182,151,226,63,4,234,127,17,190,30,248,111,195,191,2,254,8,120,195,195,190,43,159,196,122,7,196,47,217,239,224,101,253,239,136,39,240,63,197,24,227,179,240,182,239,21,235,254,41,241,119,137,116,203,221,39,81,147,193,218,175,132,180,159,17,107,122,29,223,206,96,240,121,164,113,85,106,251,127,99,135,148,228,236,253,238,101,127,229,211,167,91,175,208,214,82,131,138,92,183,105,45,126,71,231,79,236,95,251,105,254,209,191,27,126,1,252,95,248,9,251,35,248,51,192,95,2,254,32,95,126,209,223,182,143,197,223,24,124,104,248,235,241,219,246,104,241,127,141,254,16,252,56,253,179,191,108,207,218,39,246,153,253,159,62,33,124,34,253,157,190,12,248,251,226,20,127,19,181,45,115,224,199,141,141,246,131,175,248,226,247,194,158,1,190,93,87,195,222,62,240,63,252,47,31,135,183,50,67,168,246,158,27,255,0,130,81,65,226,15,26,220,252,106,248,237,241,111,87,253,160,254,59,235,150,87,250,110,181,241,151,226,207,137,215,199,30,51,139,74,190,241,5,255,0,137,238,188,39,225,6,185,185,143,79,248,107,240,200,120,162,242,255,0,83,177,240,95,132,237,116,47,6,232,151,186,165,203,232,186,13,136,42,137,251,67,251,85,254,198,95,0,255,0,108,95,9,90,232,127,23,188,37,10,120,203,195,26,127,136,33,248,71,241,199,194,182,218,62,143,241,227,224,14,185,226,54,209,166,212,188,87,240,71,226,85,222,145,117,117,224,157,82,226,235,195,126,29,254,213,178,217,115,161,120,158,199,72,26,15,139,180,127,16,120,110,234,255,0,70,187,252,32,248,255,0,251,54,127,193,71,191,96,253,58,239,197,158,15,241,47,137,255,0,110,111,217,227,79,213,109,52,251,77,79,225,183,130,188,101,63,237,135,224,91,15,16,235,218,182,157,162,47,140,126,5,124,58,209,53,75,111,141,250,117,149,146,120,116,107,222,47,240,49,210,175,238,181,79,24,75,114,62,19,120,123,194,122,86,167,226,29,47,124,215,11,153,87,187,195,87,78,142,151,166,151,43,243,214,254,247,166,157,172,16,112,86,230,90,247,251,190,227,248,37,255,0,131,128,60,1,165,252,44,255,0,130,184,126,214,62,2,209,196,73,166,120,122,63,128,80,218,44,50,199,44,107,29,239,236,197,240,95,83,42,178,70,72,108,61,235,131,201,57,7,119,205,154,43,193,191,224,173,31,29,244,63,218,111,254,10,3,241,227,227,143,135,117,253,43,197,58,71,142,173,126,15,203,111,174,104,55,246,122,166,149,121,54,131,240,43,225,151,133,175,163,181,189,177,154,72,165,104,47,244,59,171,121,20,59,24,229,180,120,159,14,140,1,94,174,22,50,142,27,15,25,233,56,211,130,123,238,162,175,248,163,54,211,108,255,0,93,207,248,36,239,252,162,203,254,9,167,255,0,102,1,251,27,255,0,235,58,252,57,175,191,232,162,186,4,20,81,69,0,127,155,63,252,23,219,254,82,213,251,88,255,0,221,9,255,0,214,105,248,55,69,20,80,7,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 2524; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

