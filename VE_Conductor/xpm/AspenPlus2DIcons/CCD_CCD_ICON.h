#ifndef GETVESUITE_CCD_CCD_ICON_H
#define GETVESUITE_CCD_CCD_ICON_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_CCD_CCD_ICON( void )
{
    unsigned char osgData[ 3742 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,71,0,31,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,253,76,253,188,191,224,178,95,180,111,196,143,218,3,226,15,236,161,255,0,4,237,213,116,239,134,30,27,248,65,227,31,18,124,39,248,195,251,77,235,223,15,116,207,22,252,74,212,190,50,252,61,241,189,166,149,227,255,0,9,124,7,240,143,196,91,107,191,14,248,95,194,30,28,213,188,53,226,47,13,107,158,34,241,119,134,60,73,255,0,9,45,222,171,169,167,133,52,141,38,199,72,209,188,109,175,252,35,166,255,0,193,52,181,223,218,66,199,195,67,246,148,241,119,198,111,218,186,231,65,138,250,63,11,222,254,211,63,24,126,38,126,209,208,248,18,93,66,218,213,252,73,47,130,23,227,135,137,117,245,240,90,235,13,165,104,103,83,26,105,211,197,249,208,244,228,188,75,129,97,103,229,126,50,126,194,118,63,180,223,140,244,221,31,227,78,189,241,250,239,195,30,20,241,125,189,159,198,31,141,191,24,252,77,241,67,224,70,133,226,107,65,227,41,135,141,254,50,252,77,215,237,126,35,254,193,158,45,131,89,214,151,80,212,60,83,170,93,220,235,62,46,136,234,114,70,215,122,182,179,104,215,23,19,65,253,73,124,30,240,7,199,29,95,225,239,195,127,141,191,1,191,224,167,94,48,241,119,236,197,228,248,167,196,223,28,62,60,235,191,25,255,0,224,156,254,6,240,111,195,15,134,158,11,212,196,94,57,241,223,129,117,77,55,254,9,55,227,237,3,226,59,105,26,118,141,241,14,77,82,13,99,196,222,12,211,236,39,240,138,216,93,235,80,195,115,125,117,163,252,238,99,136,196,115,181,79,21,26,80,189,149,185,173,117,210,77,45,223,69,215,161,180,18,211,75,223,95,200,248,46,111,216,127,227,15,236,147,109,19,254,201,223,24,190,60,254,202,176,104,94,41,182,241,181,143,131,254,9,124,90,241,175,195,207,132,218,175,142,244,123,157,54,85,241,183,139,126,6,105,55,177,248,31,226,94,173,117,105,161,248,126,195,80,79,19,248,107,94,177,214,52,205,26,207,74,214,97,212,52,123,120,172,143,234,207,252,18,207,254,11,1,241,103,226,127,198,223,15,254,195,127,183,69,142,135,115,241,199,198,54,58,141,183,236,245,241,203,192,158,12,187,240,206,155,241,146,235,225,207,195,219,223,20,120,243,193,127,25,124,27,166,234,23,150,94,12,248,202,124,47,224,223,22,120,182,47,16,104,176,232,254,6,241,5,186,106,154,85,166,133,224,173,67,75,208,244,175,21,252,215,251,75,120,87,196,94,32,248,15,226,15,143,95,10,127,224,171,159,24,126,54,252,56,139,195,62,57,241,47,135,126,34,79,170,255,0,193,53,27,224,142,180,222,13,179,215,45,181,139,173,99,226,215,195,175,216,46,242,199,72,240,197,174,187,162,106,182,186,214,167,4,58,143,246,103,246,109,235,205,105,61,197,164,144,87,241,235,251,103,124,122,248,135,224,205,38,15,141,63,3,127,110,95,21,107,223,23,254,30,77,162,222,39,142,254,6,252,75,248,32,186,239,195,191,17,120,178,212,120,67,93,95,9,252,96,253,158,254,16,248,47,93,211,173,238,244,45,119,198,90,80,190,132,104,179,107,58,93,197,221,189,221,146,90,220,222,88,85,229,149,177,18,147,140,241,10,172,19,179,77,74,233,250,180,182,251,133,36,154,218,207,254,7,252,3,237,127,248,38,125,143,199,237,127,225,63,192,200,252,41,240,203,225,182,183,167,120,91,79,248,105,226,191,14,220,221,126,215,63,27,190,9,94,234,119,90,37,172,103,75,143,197,150,95,12,254,1,106,113,235,218,12,171,112,223,110,208,53,43,141,87,68,187,194,173,229,181,240,138,45,191,213,31,134,254,24,126,219,223,30,180,111,4,197,226,15,217,211,224,229,140,62,21,179,213,127,178,31,225,63,252,22,159,254,10,35,251,40,203,118,186,212,86,95,108,111,19,55,236,205,251,22,232,77,226,226,131,72,182,251,32,213,158,252,105,254,109,208,211,205,176,191,190,251,71,243,5,255,0,4,169,253,161,60,3,224,127,133,31,10,236,117,189,3,227,133,244,246,30,8,240,93,180,242,120,67,246,98,253,164,190,33,217,201,37,189,157,162,200,214,154,143,128,126,19,106,118,250,141,185,40,76,114,219,203,44,82,130,12,78,225,129,63,215,167,236,225,255,0,5,2,248,19,163,233,58,106,93,248,11,246,219,153,163,178,85,102,210,191,224,154,31,240,81,237,117,9,242,167,0,44,186,39,236,169,112,174,57,31,50,146,49,159,66,107,207,199,188,84,43,191,101,132,231,148,91,229,126,247,93,254,209,165,53,27,107,43,93,43,109,228,126,113,254,214,95,2,191,107,91,191,0,120,67,192,254,35,248,23,224,157,47,67,248,114,190,47,26,6,161,161,255,0,193,109,63,224,165,122,207,140,175,135,139,46,163,214,181,99,227,175,31,221,126,203,218,110,189,241,80,197,117,10,174,153,255,0,9,70,169,171,255,0,99,90,49,211,180,115,97,96,239,110,223,201,39,252,21,67,254,22,222,159,240,239,83,208,60,105,224,111,1,248,127,73,209,255,0,225,31,210,162,187,209,190,58,252,66,248,193,175,52,26,110,173,225,155,91,55,212,53,191,31,124,34,209,175,252,75,126,255,0,103,182,251,69,254,161,125,37,237,195,203,45,205,204,183,19,150,50,255,0,109,191,181,119,237,211,240,79,93,211,239,99,177,240,71,237,145,9,97,117,143,237,143,248,39,87,252,20,23,195,170,55,217,21,92,191,136,63,102,43,85,140,96,130,115,141,160,110,108,14,107,248,164,255,0,130,181,124,93,240,167,142,252,39,226,152,52,61,43,226,117,140,147,106,122,107,70,124,103,240,79,227,47,195,136,0,79,16,120,126,102,18,221,124,67,240,22,151,29,187,108,137,246,172,142,165,152,170,0,93,209,91,167,45,158,42,117,35,26,184,111,102,175,127,181,189,239,173,223,93,254,100,207,147,236,187,164,191,29,143,175,191,224,159,31,5,252,47,171,252,17,248,69,241,107,195,158,8,248,15,105,241,79,193,30,15,248,115,227,163,241,23,226,31,236,181,168,126,210,30,46,130,15,5,120,114,27,203,41,124,33,225,175,1,120,175,64,241,62,161,227,203,107,173,47,66,125,37,180,187,251,203,252,233,43,99,167,233,247,23,115,218,181,191,246,53,255,0,4,212,241,127,137,127,104,175,5,252,91,240,23,199,171,253,7,226,247,129,111,252,41,163,248,79,82,240,111,138,191,224,156,223,181,79,236,125,225,125,91,194,222,54,210,188,113,164,248,199,195,126,35,177,253,174,124,107,226,13,59,227,246,129,169,233,48,199,109,121,105,163,65,5,174,151,111,230,193,174,69,117,22,187,167,172,127,200,199,252,18,171,246,100,253,155,190,33,252,41,248,87,168,120,255,0,246,124,248,31,227,157,71,80,240,79,130,174,181,27,255,0,24,124,39,240,31,137,175,47,174,174,172,173,90,234,234,246,235,89,240,252,239,117,60,140,196,201,35,179,59,146,75,22,228,143,235,211,246,111,255,0,130,103,255,0,193,56,117,221,39,77,147,90,255,0,130,127,126,196,154,204,146,89,9,30,77,87,246,83,248,19,168,59,183,149,112,119,179,221,248,9,203,54,64,234,79,220,207,53,199,154,98,112,235,218,83,171,41,243,166,237,37,24,222,58,167,166,183,233,209,142,9,251,173,91,109,87,220,124,129,251,73,126,203,82,126,202,254,33,248,143,226,239,217,175,194,223,177,79,236,251,240,7,81,26,30,185,227,79,134,127,4,255,0,224,159,190,32,211,254,51,248,179,195,62,5,240,236,183,122,198,145,117,227,159,131,31,30,244,200,124,117,226,150,158,255,0,198,205,225,181,183,248,121,121,62,156,124,81,29,156,26,70,181,116,39,109,75,248,174,255,0,130,164,218,252,15,241,46,135,241,47,198,223,15,190,20,183,132,60,71,115,226,91,45,95,92,241,31,136,255,0,103,207,24,124,24,241,70,175,170,248,147,197,90,109,198,163,169,73,119,241,3,225,254,137,125,175,106,87,51,190,163,37,244,169,231,202,26,247,117,219,131,114,134,95,238,31,246,174,255,0,130,117,255,0,193,62,252,59,167,95,73,225,239,216,87,246,56,208,93,22,232,7,209,191,102,31,130,154,107,38,44,203,38,211,101,224,136,206,119,130,71,32,231,35,174,49,252,81,255,0,193,90,254,9,124,25,248,113,225,63,20,92,252,59,248,71,240,199,192,87,54,218,150,156,150,215,30,11,240,23,133,124,49,60,9,39,136,124,61,4,137,4,218,22,149,3,34,27,121,230,70,3,0,172,204,14,67,17,93,57,86,34,141,90,148,210,156,231,81,36,174,210,87,95,222,179,119,249,254,162,156,90,77,89,37,110,231,178,126,193,126,58,214,126,24,252,57,248,43,125,226,255,0,142,31,180,39,193,95,135,90,254,155,240,195,195,209,124,81,183,241,55,252,19,139,194,31,10,60,63,55,136,52,171,75,247,65,123,251,69,233,103,94,189,180,211,172,32,213,166,154,222,61,63,82,213,230,131,195,183,114,218,91,106,65,35,51,127,87,190,10,248,163,240,187,225,95,193,63,13,124,98,215,127,224,185,63,21,116,95,11,248,203,194,254,61,213,254,17,197,99,241,59,254,8,193,225,75,95,141,250,159,195,101,184,177,241,63,132,62,17,120,199,227,111,236,159,165,248,111,196,62,32,131,196,111,101,163,220,73,117,226,11,61,55,77,212,245,107,120,181,189,79,76,141,154,120,191,156,207,216,74,211,246,177,248,85,225,253,111,246,123,181,248,121,240,151,195,95,18,254,17,75,226,191,128,62,49,187,212,191,104,61,103,70,241,79,195,255,0,137,223,9,181,173,87,225,183,141,69,139,88,126,201,222,61,240,198,165,174,232,158,51,208,117,53,138,84,127,17,120,126,230,125,49,37,133,245,157,54,64,110,63,166,143,216,95,196,95,240,83,47,134,54,122,195,107,90,103,192,79,218,22,223,88,143,74,131,72,143,227,143,237,215,165,248,58,31,6,141,37,53,127,182,191,135,99,253,159,255,0,224,139,254,26,150,68,212,83,85,180,123,161,173,75,171,149,26,29,168,177,54,79,37,210,222,94,58,202,110,82,80,139,167,39,116,220,47,37,255,0,111,69,217,237,125,87,94,161,22,149,180,110,235,207,181,207,132,227,248,201,226,31,218,215,193,159,25,60,67,240,195,246,226,253,182,252,103,117,240,159,73,211,181,27,207,11,175,196,79,248,32,159,197,253,95,197,87,94,35,210,188,89,115,163,104,154,93,231,236,139,224,239,22,105,126,22,213,46,166,240,149,221,180,15,226,189,83,195,54,87,50,222,35,90,94,77,13,174,171,62,157,252,142,127,193,69,254,34,167,138,116,143,136,186,14,181,241,115,226,142,167,226,175,13,248,138,211,71,212,188,11,241,91,197,95,177,78,173,226,57,53,107,31,19,216,218,107,90,125,222,151,251,48,45,204,250,118,169,166,93,105,211,125,178,27,137,108,218,41,97,8,18,81,29,218,71,253,168,254,215,154,47,252,20,211,196,159,17,174,252,119,127,175,252,36,240,119,131,229,213,116,155,227,240,79,194,31,182,127,131,60,77,240,190,27,77,19,76,211,45,53,125,17,117,31,19,255,0,193,22,23,199,19,105,26,180,246,23,114,234,18,15,23,181,234,205,174,93,13,30,247,76,183,22,41,105,252,210,254,219,31,179,215,237,145,251,102,252,76,240,231,236,195,240,207,225,7,194,79,17,124,110,248,201,226,13,83,78,248,127,224,47,4,124,122,191,190,188,241,6,163,224,207,14,106,127,22,60,77,109,14,191,241,111,225,15,129,116,29,61,45,60,13,240,247,196,247,207,38,163,171,216,164,169,167,53,189,153,185,191,150,214,206,231,108,11,135,183,78,49,166,163,59,52,162,213,227,107,93,104,181,215,254,3,20,174,147,223,78,255,0,113,253,149,126,223,223,240,68,13,115,226,111,198,63,17,126,213,95,176,87,142,254,29,124,22,248,187,227,109,65,252,77,241,111,224,127,196,123,47,17,232,223,3,190,48,124,70,212,188,67,164,201,172,124,88,210,60,111,224,123,45,78,255,0,224,47,196,43,237,26,227,196,151,190,41,150,207,194,126,44,210,252,111,171,216,233,247,183,90,78,131,226,13,71,196,222,47,213,255,0,18,254,57,254,218,191,180,95,252,19,58,203,225,178,127,193,65,254,8,248,215,246,98,190,248,165,111,226,65,240,142,227,89,241,127,194,223,137,195,198,247,62,6,143,66,95,137,70,196,254,205,255,0,17,124,102,186,44,90,105,241,215,131,87,254,39,81,233,127,104,77,122,33,167,139,207,179,95,125,144,162,189,76,70,3,11,137,124,213,105,222,93,214,140,205,73,173,142,171,224,79,198,15,219,59,254,10,135,224,219,159,27,254,193,63,2,181,63,139,63,9,91,226,60,127,9,188,83,241,195,196,95,18,126,26,252,49,248,121,224,15,137,62,71,135,117,207,19,167,142,52,159,136,190,45,182,241,204,222,31,210,252,29,227,207,10,106,250,133,231,134,60,15,226,89,26,219,80,242,244,123,93,99,87,183,184,210,147,247,179,254,9,151,255,0,4,111,210,63,100,47,26,183,237,53,251,74,248,223,195,63,31,255,0,107,107,157,33,108,188,27,127,160,120,119,82,210,254,21,126,205,86,62,36,240,213,189,151,196,29,27,225,28,30,36,212,110,47,188,105,227,237,70,246,251,196,58,86,161,241,27,82,182,209,53,61,75,195,30,70,145,163,248,95,193,118,58,167,138,244,255,0,16,148,83,195,224,240,248,116,189,149,59,62,251,189,65,202,82,221,159,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 3742; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

