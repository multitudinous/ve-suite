#ifndef GETVESUITE_Mixer_Mixer_WORK_H
#define GETVESUITE_Mixer_Mixer_WORK_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Mixer_Mixer_WORK( void )
{
    unsigned char osgData[ 3766 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,95,0,95,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,250,79,246,122,255,0,130,115,254,193,223,15,252,83,255,0,6,238,254,211,127,16,127,100,79,217,195,197,95,15,63,108,223,216,71,192,31,178,199,199,45,23,198,31,11,188,13,173,248,35,196,63,180,239,141,63,101,15,4,124,126,248,35,241,131,196,191,15,181,223,14,220,104,190,40,248,145,173,220,248,27,227,78,145,169,235,211,192,53,187,203,239,18,105,115,92,79,124,202,210,218,253,11,255,0,5,252,255,0,130,100,255,0,193,58,255,0,103,207,217,175,224,55,237,203,224,239,217,35,246,102,248,69,225,207,216,219,246,201,253,150,254,35,124,122,240,143,195,15,128,127,13,252,31,163,252,118,253,157,60,71,241,147,194,158,2,248,145,240,159,197,126,20,240,126,149,166,91,120,202,43,155,143,19,248,126,236,71,119,13,241,107,45,35,80,176,16,172,55,243,92,91,253,1,241,79,225,135,141,60,125,255,0,6,203,126,200,126,53,248,84,144,55,198,143,217,111,246,13,255,0,130,123,254,218,159,6,36,159,195,247,94,40,63,240,177,63,100,111,134,31,6,126,59,217,105,150,218,38,159,125,107,117,121,115,170,104,94,11,215,180,101,251,37,196,119,11,255,0,9,17,104,150,118,31,102,155,244,122,120,190,17,127,193,83,52,175,14,165,196,118,62,58,253,145,103,253,159,45,252,77,171,217,134,181,213,124,59,227,239,136,127,181,79,193,214,139,195,186,68,236,110,10,94,191,132,254,1,124,65,212,245,27,171,75,235,3,3,223,252,108,240,222,165,106,233,127,161,186,64,1,193,124,125,255,0,130,112,127,193,44,52,127,217,215,199,94,51,240,103,252,19,147,254,9,241,123,171,120,159,193,112,104,127,11,46,109,255,0,101,63,128,62,31,135,196,30,63,248,167,246,47,5,252,32,211,172,60,67,167,124,56,251,86,147,46,171,227,191,22,120,82,210,218,234,213,77,212,82,106,145,201,108,143,56,141,27,209,126,24,127,193,24,255,0,224,149,127,12,62,28,248,23,225,205,183,252,19,219,246,49,241,124,30,6,240,158,131,225,84,241,103,196,47,217,143,224,199,143,60,117,226,83,161,233,182,218,123,235,254,46,241,159,139,188,27,125,169,248,155,196,119,143,3,79,119,121,125,119,113,113,60,243,187,201,35,19,199,192,159,240,72,95,137,222,39,241,207,192,255,0,217,255,0,254,9,229,241,63,80,91,207,140,223,240,76,255,0,141,31,24,126,4,124,127,177,255,0,132,75,91,209,116,221,99,192,255,0,177,108,250,111,130,255,0,102,77,117,109,53,203,187,129,103,101,175,104,127,18,127,103,207,21,104,55,203,113,61,174,172,126,28,234,183,26,65,48,217,76,108,255,0,163,90,0,248,3,254,29,59,255,0,4,178,255,0,164,105,254,192,31,248,134,255,0,179,175,255,0,59,154,63,225,211,191,240,75,47,250,70,159,236,1,255,0,136,111,251,58,255,0,243,185,175,191,232,160,15,128,63,225,211,191,240,75,47,250,70,159,236,1,255,0,136,111,251,58,255,0,243,185,163,254,29,59,255,0,4,178,255,0,164,105,254,192,31,248,134,255,0,179,175,255,0,59,154,251,254,138,0,248,3,254,29,59,255,0,4,178,255,0,164,105,254,192,31,248,134,255,0,179,175,255,0,59,154,63,225,211,191,240,75,47,250,70,159,236,1,255,0,136,111,251,58,255,0,243,185,175,191,232,160,15,128,63,225,211,191,240,75,47,250,70,159,236,1,255,0,136,111,251,58,255,0,243,185,175,196,31,248,56,239,254,9,237,251,3,124,14,255,0,130,49,254,217,31,20,126,10,126,195,223,178,7,193,255,0,137,190,23,255,0,134,121,255,0,132,103,226,47,194,223,217,167,224,199,195,255,0,30,120,116,235,127,181,95,192,223,14,235,95,216,94,46,240,151,130,173,53,13,35,237,126,30,213,181,91,27,175,179,220,71,246,139,61,78,226,214,93,208,77,34,55,245,123,95,128,63,240,116,119,252,160,163,246,230,255,0,187,102,255,0,214,195,253,159,104,3,239,15,248,37,142,153,97,173,127,193,39,255,0,224,156,154,54,171,107,21,246,151,171,127,193,60,191,100,45,51,82,178,156,22,130,242,194,255,0,246,109,248,123,107,121,107,50,130,51,20,150,242,202,140,1,232,230,189,191,246,51,253,145,126,14,254,194,95,179,63,194,111,217,75,224,54,159,169,217,252,50,248,65,225,230,208,244,107,189,126,93,42,235,197,62,35,190,189,190,187,214,124,67,226,255,0,24,106,26,46,143,167,218,234,126,45,213,181,253,71,81,191,212,110,45,236,109,32,146,234,254,79,34,218,222,17,28,49,248,255,0,252,18,119,254,81,101,255,0,4,211,255,0,179,0,253,141,255,0,245,157,126,28,215,223,244,1,243,39,195,63,217,19,224,119,194,47,218,59,246,148,253,170,252,15,225,169,180,191,140,127,181,141,143,193,237,63,227,54,182,110,145,236,117,168,190,7,248,123,91,240,191,130,39,211,244,212,183,85,211,111,14,147,175,92,165,252,193,158,75,230,179,180,51,177,22,144,4,250,110,138,40,0,162,138,40,0,162,138,40,0,162,138,40,0,175,192,31,248,58,59,254,80,81,251,115,127,221,179,127,235,97,254,207,181,251,253,95,128,63,240,116,119,252,160,163,246,230,255,0,187,102,255,0,214,195,253,159,104,3,239,255,0,248,36,239,252,162,203,254,9,167,255,0,102,1,251,27,255,0,235,58,252,57,175,191,235,224,15,248,36,239,252,162,203,254,9,167,255,0,102,1,251,27,255,0,235,58,252,57,175,191,232,0,162,138,40,0,162,138,40,0,162,138,40,0,162,138,40,0,175,192,31,248,58,59,254,80,81,251,115,127,221,179,127,235,97,254,207,181,251,253,95,128,63,240,116,119,252,160,163,246,230,255,0,187,102,255,0,214,195,253,159,104,3,239,255,0,248,36,239,252,162,203,254,9,167,255,0,102,1,251,27,255,0,235,58,252,57,175,191,235,224,15,248,36,239,252,162,203,254,9,167,255,0,102,1,251,27,255,0,235,58,252,57,175,191,232,0,162,138,40,0,162,138,40,0,162,138,40,0,162,138,40,0,175,192,31,248,58,59,254,80,81,251,115,127,221,179,127,235,97,254,207,181,251,253,95,128,63,240,116,119,252,160,163,246,230,255,0,187,102,255,0,214,195,253,159,104,3,239,255,0,248,36,239,252,162,203,254,9,167,255,0,102,1,251,27,255,0,235,58,252,57,175,191,235,224,15,248,36,239,252,162,203,254,9,167,255,0,102,1,251,27,255,0,235,58,252,57,175,191,232,0,162,138,241,253,107,246,133,248,7,225,207,140,190,19,253,156,252,65,241,199,224,254,133,251,65,248,247,195,247,62,45,240,47,192,157,103,226,95,131,52,207,140,190,52,240,173,156,30,38,186,188,241,47,132,254,23,222,235,81,235,126,35,240,252,86,222,11,241,140,146,94,89,216,205,110,145,248,79,83,118,144,45,133,209,136,3,216,40,175,159,254,35,126,214,63,178,207,193,255,0,29,193,240,183,226,215,237,45,240,3,225,119,196,219,159,135,254,33,248,177,109,240,235,226,47,198,79,135,94,8,241,221,199,194,207,8,104,254,46,241,23,139,126,37,193,225,31,18,248,142,215,80,155,192,26,95,135,254,31,248,242,251,81,214,86,220,233,182,54,126,9,213,238,174,174,98,131,77,188,120,121,255,0,3,126,219,191,177,127,196,239,20,252,54,240,47,195,95,218,243,246,96,248,135,227,111,140,190,31,215,60,89,240,131,193,222,7,248,251,240,167,197,190,42,248,173,225,95,12,106,62,54,209,252,75,226,111,134,222,30,208,60,89,113,119,227,175,15,233,250,191,195,79,136,214,183,215,186,92,55,86,214,151,62,0,214,224,158,72,229,210,175,146,0,15,168,40,175,128,63,225,236,95,240,75,47,250,73,103,236,1,255,0,137,145,251,58,255,0,243,199,173,253,107,254,10,107,255,0,4,220,240,238,157,225,61,99,196,63,240,80,111,216,131,66,210,60,123,225,251,159,22,120,23,84,214,191,106,255,0,128,218,102,157,227,79,10,217,248,171,196,222,5,188,241,55,132,239,175,124,124,145,120,143,195,241,120,219,193,126,49,209,164,189,179,105,173,163,213,124,39,169,233,239,32,187,176,186,134,32,15,184,40,174,127,194,126,44,240,175,143,124,43,225,159,29,120,23,196,222,31,241,167,130,124,105,225,253,27,197,158,14,241,143,132,245,157,59,196,126,21,241,103,133,124,71,167,91,107,30,30,241,55,134,124,67,163,220,205,105,174,248,126,255,0,72,188,180,186,178,188,181,154,91,107,171,107,168,231,130,89,34,117,99,208,80,1,95,128,63,240,116,119,252,160,163,246,230,255,0,187,102,255,0,214,195,253,159,107,247,250,191,0,127,224,232,239,249,65,71,237,205,255,0,118,205,255,0,173,135,251,62,208,7,223,255,0,240,73,223,249,69,151,252,19,79,254,204,3,246,55,255,0,214,117,248,115,95,127,215,242,133,255,0,4,246,255,0,131,143,63,224,140,127,3,191,96,95,216,123,224,167,197,47,219,39,254,17,127,137,191,7,255,0,100,15,217,167,225,111,196,95,13,127,195,60,254,213,122,215,252,35,190,59,248,127,240,99,193,94,19,241,118,133,253,179,225,223,129,183,122,126,173,246,79,16,105,26,133,191,218,172,110,238,172,238,62,207,230,218,220,77,3,164,141,245,255,0,252,69,29,255,0,4,40,255,0,163,230,255,0,205,102,253,176,255,0,250,31,104,3,247,250,191,158,31,248,43,39,139,60,44,159,183,79,236,156,158,10,241,55,135,245,191,218,195,246,117,253,136,63,109,255,0,218,239,224,63,193,47,11,235,58,116,223,180,63,197,79,21,124,24,253,164,127,224,156,159,27,7,192,143,132,122,6,147,115,47,137,116,141,127,227,95,192,143,217,219,246,177,248,91,246,253,27,77,213,46,111,60,37,175,252,68,139,251,19,197,26,62,155,226,95,15,221,244,31,241,20,119,252,16,163,254,143,155,255,0,53,155,246,195,255,0,232,125,163,254,34,142,255,0,130,20,127,209,243,127,230,179,126,216,127,253,15,180,1,227,255,0,4,191,104,79,128,80,127,193,87,188,55,251,84,252,91,248,225,240,127,192,122,183,237,25,251,48,127,193,84,252,89,224,24,62,35,124,75,240,94,153,168,248,115,246,47,253,159,62,48,127,193,61,190,23,126,207,94,37,240,127,141,188,75,173,218,203,226,95,217,127,199,158,9,253,143,255,0,107,95,218,79,193,151,150,86,246,126,20,139,74,248,219,241,23,91,209,36,214,109,52,239,18,248,211,90,243,255,0,132,159,29,126,30,252,83,255,0,130,110,127,193,33,127,103,31,216,187,246,132,248,63,161,255,0,193,87,124,103,255,0,4,224,248,118,159,177,231,140,180,223,141,31,12,252,57,113,240,147,78,248,125,240,27,194,94,10,253,163,111,190,45,127,108,248,35,199,17,124,66,248,62,158,54,248,81,169,248,127,94,248,94,124,21,226,237,87,196,158,50,248,68,186,157,142,139,225,157,71,225,38,181,241,103,224,255,0,212,31,241,20,119,252,16,163,254,143,155,255,0,53,155,246,195,255,0,232,125,163,254,34,142,255,0,130,20,127,209,243,127,230,179,126,216,127,253,15,180,1,243,7,197,239,248,40,143,252,19,211,196,30,7,255,0,130,86,217,254,197,255,0,183,63,236,65,251,58,234,223,15,127,109,255,0,138,62,38,241,44,95,180,143,197,79,0,252,108,159,224,198,163,63,236,75,255,0,5,22,240,119,197,255,0,24,126,210,154,62,133,251,89,232,58,151,197,15,16,107,255,0,24,252,99,169,217,93,252,74,63,18,245,13,43,199,190,50,248,169,166,248,234,199,198,190,62,180,241,69,134,161,226,99,195,31,180,7,138,188,103,241,247,254,9,195,240,203,246,49,253,177,191,224,156,62,38,253,184,244,239,248,39,7,252,20,36,120,251,197,90,239,130,181,31,24,254,204,31,181,111,237,63,227,143,26,127,193,61,254,46,252,100,241,71,193,193,240,51,198,191,13,27,196,62,31,241,247,237,7,224,47,218,7,95,212,126,34,120,55,75,241,70,159,107,168,124,34,248,183,97,115,225,27,239,23,120,63,198,58,87,134,126,159,255,0,136,163,191,224,133,31,244,124,223,249,172,223,182,31,255,0,67,237,31,241,20,119,252,16,163,254,143,155,255,0,53,155,246,195,255,0,232,125,160,15,191,255,0,224,153,158,36,248,87,226,79,217,59,195,215,159,13,188,127,255,0,9,175,136,53,63,136,31,23,126,36,126,208,58,22,169,103,225,207,11,248,239,224,231,237,77,251,70,252,73,241,39,237,83,251,68,252,8,248,165,240,155,65,241,175,136,231,253,159,126,32,120,83,226,111,199,77,115,77,184,248,121,173,235,250,239,136,188,27,103,21,142,139,174,107,222,32,212,45,166,215,117,47,191,235,240,7,254,34,142,255,0,130,20,127,209,243,127,230,179,126,216,127,253,15,180,127,196,81,223,240,66,143,250,62,111,252,214,111,219,15,255,0,161,246,128,63,127,171,240,7,254,14,142,255,0,148,20,126,220,223,247,108,223,250,216,127,179,237,31,241,20,119,252,16,163,254,143,155,255,0,53,155,246,195,255,0,232,125,175,200,31,248,47,87,252,23,171,254,9,59,251,104,255,0,193,39,191,106,207,217,163,246,105,253,171,63,225,100,252,108,248,149,255,0,10,55,254,16,175,5,127,194,140,253,164,252,29,253,181,255,0,8,119,237,37,240,119,199,254,36,255,0,138,147,199,223,7,116,189,39,78,251,55,132,188,45,175,93,255,0,165,223,193,231,125,131,236,246,254,109,204,176,195,32,7,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 3766; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

