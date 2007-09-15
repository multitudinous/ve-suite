#ifndef GETVESUITE_MHeatX_MHeatX_SIMP-MHX_H
#define GETVESUITE_MHeatX_MHeatX_SIMP-MHX_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_MHeatX_MHeatX_SIMP-MHX( void )
{
    unsigned char osgData[ 3075 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,49,0,49,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,247,124,75,226,95,14,120,47,195,158,32,241,143,140,124,65,162,120,79,194,62,19,209,53,95,18,248,167,197,62,37,213,108,52,47,14,120,107,195,154,21,132,250,166,185,226,15,16,107,154,164,241,90,232,218,37,158,153,107,117,115,119,119,115,44,112,91,193,109,36,211,72,145,163,48,254,71,127,111,111,248,56,163,196,190,63,188,190,248,61,255,0,4,198,183,184,180,210,231,30,9,212,101,253,179,124,91,225,141,134,117,149,175,117,111,19,120,99,225,127,193,47,138,158,2,96,145,42,195,162,216,220,248,139,197,86,129,89,98,215,236,180,207,13,252,250,23,139,213,191,240,113,63,237,243,117,241,3,197,16,255,0,193,48,190,14,223,25,244,219,25,188,43,226,255,0,219,58,109,75,192,243,201,20,249,62,5,248,167,240,71,225,119,133,188,89,171,93,162,42,170,255,0,101,248,171,196,115,216,89,175,200,190,25,211,109,53,231,15,226,221,10,191,17,126,13,216,248,91,195,62,126,175,117,225,123,217,190,19,120,66,238,95,15,252,69,248,203,110,246,35,194,158,14,241,13,188,173,101,125,113,123,106,168,27,83,240,86,135,253,157,121,101,227,31,17,91,22,211,188,31,127,127,18,106,18,203,101,164,120,251,81,240,39,6,43,20,233,183,78,159,196,150,175,183,101,235,228,181,185,113,138,122,179,205,60,107,240,171,246,138,253,163,46,60,13,39,237,23,241,203,227,167,199,195,225,59,77,89,252,52,191,26,126,43,120,255,0,226,132,94,28,147,196,86,58,123,120,129,252,63,23,141,181,251,228,209,13,244,154,46,140,110,254,202,145,11,131,164,90,153,67,125,158,16,145,120,47,224,23,198,15,128,62,50,159,198,31,0,126,38,252,87,248,27,226,189,75,195,18,248,107,82,241,55,193,207,136,30,48,248,99,175,234,30,30,189,213,109,117,75,205,10,255,0,89,240,78,175,99,113,119,163,203,169,104,218,53,196,182,178,72,208,60,250,69,172,172,133,237,226,41,253,13,120,35,246,111,138,104,236,75,216,69,189,4,153,88,226,224,121,144,72,237,149,36,129,137,94,85,246,217,131,206,77,107,248,191,246,110,182,137,37,115,96,128,53,151,148,3,168,65,153,30,100,36,49,92,29,168,89,136,32,228,39,39,25,199,206,188,206,106,90,205,169,122,154,114,43,124,58,35,231,31,248,39,255,0,252,23,187,227,247,236,203,170,120,95,224,151,252,20,8,248,151,227,231,194,27,173,103,195,30,31,179,253,165,17,96,159,226,223,193,127,10,88,248,94,77,14,91,207,30,104,58,15,133,141,199,199,253,9,53,61,43,68,187,191,191,146,127,248,78,33,243,124,67,169,92,93,120,218,254,235,73,209,45,191,177,95,130,191,26,190,21,126,209,159,10,188,15,241,187,224,143,142,52,79,136,255,0,10,254,35,232,145,120,131,193,222,49,208,37,153,172,117,75,7,154,107,75,171,123,139,91,200,98,186,209,181,187,61,78,214,250,199,83,211,47,160,182,212,180,173,75,77,187,211,117,43,75,75,251,75,155,104,191,132,255,0,143,223,179,181,168,178,190,119,176,137,165,204,152,67,1,72,86,64,86,97,146,98,101,73,18,231,123,6,148,48,217,51,96,149,57,95,93,255,0,130,22,254,220,247,31,177,47,237,57,63,236,103,241,70,244,217,126,206,191,181,111,143,237,34,248,113,46,155,224,71,213,117,47,10,126,214,62,45,186,240,95,129,60,47,117,169,120,131,67,187,251,69,159,129,252,89,164,105,58,118,133,125,230,218,234,112,216,107,186,103,135,111,34,77,7,74,159,197,58,164,222,230,7,31,237,146,132,229,119,223,238,254,174,103,40,218,237,108,127,116,148,81,69,122,164,31,229,91,224,175,26,107,191,182,87,237,35,241,243,227,22,143,225,121,190,27,120,87,227,175,199,207,139,159,22,188,101,62,149,226,24,239,252,65,167,47,196,175,136,58,255,0,140,110,62,21,248,31,198,246,182,90,117,196,183,86,137,173,141,63,87,241,85,181,181,133,213,146,89,73,103,162,71,99,226,41,39,191,240,151,244,171,251,45,252,50,209,244,125,31,195,250,54,137,160,105,250,30,145,163,233,90,118,141,163,105,90,85,190,159,103,166,105,58,86,159,108,150,154,110,159,167,88,219,170,165,142,159,111,103,28,49,195,12,62,84,113,71,18,164,106,128,87,243,221,251,61,248,23,197,159,179,207,197,159,137,31,179,199,138,27,194,119,94,35,248,11,241,75,199,159,5,117,235,191,9,157,74,127,12,92,235,95,11,60,93,171,248,23,84,185,240,237,198,163,103,97,60,218,11,223,104,19,181,163,205,101,103,43,219,152,204,150,182,238,198,21,254,141,127,102,239,23,199,28,22,69,164,179,109,177,164,159,187,89,100,5,84,96,252,226,93,170,197,71,241,17,140,238,56,28,215,200,102,147,155,169,53,110,88,167,183,158,154,255,0,91,108,141,224,147,73,119,62,169,240,15,128,161,253,141,161,183,189,189,181,70,253,139,202,70,218,133,251,196,196,126,197,91,131,121,151,247,210,238,42,255,0,177,86,201,93,174,46,25,137,248,42,199,207,159,31,5,68,211,124,21,251,211,226,63,194,187,123,43,105,22,75,88,243,179,98,149,132,176,32,35,2,25,143,203,180,180,178,171,2,84,128,51,144,71,39,194,159,136,182,214,144,91,72,178,166,124,184,190,64,98,80,23,1,152,161,220,197,135,150,178,16,67,231,112,35,130,0,63,34,248,231,198,240,254,198,118,239,103,102,233,255,0,12,72,83,22,22,10,209,183,252,49,47,200,74,217,90,46,67,55,236,66,210,36,139,12,32,143,248,82,1,124,152,113,240,71,200,135,224,143,128,220,177,19,237,93,127,228,246,183,254,77,255,0,165,255,0,139,226,232,73,69,89,108,124,139,251,68,248,30,40,98,189,97,98,38,145,22,112,172,100,129,48,95,118,24,63,155,191,32,29,163,46,220,18,56,7,143,230,119,246,230,240,108,242,233,122,199,145,163,218,59,164,50,152,93,141,137,149,93,23,49,200,37,150,92,249,129,130,157,196,238,207,57,207,53,253,60,126,209,62,48,142,91,123,247,142,75,69,220,179,145,230,43,133,194,22,31,235,60,226,140,119,116,1,137,199,110,13,127,42,31,183,23,196,205,87,198,195,197,154,63,131,230,134,63,9,104,55,58,150,147,226,159,26,88,181,202,127,106,235,90,93,196,214,186,175,129,188,23,127,5,200,50,220,218,222,91,207,107,175,235,22,236,23,75,184,138,109,19,78,153,188,67,22,169,63,133,189,156,178,53,29,72,107,102,154,191,150,221,76,101,107,190,199,239,119,252,70,20,127,233,29,195,255,0,18,224,127,244,50,209,95,19,127,196,37,255,0,240,81,175,250,45,95,177,63,254,28,95,142,191,253,14,52,87,215,90,175,243,126,8,230,62,203,255,0,130,246,255,0,193,63,181,111,217,147,246,129,111,248,40,47,193,63,13,107,119,127,7,254,62,248,142,40,191,105,45,63,64,240,191,133,44,124,35,240,83,226,236,177,120,103,65,208,252,127,117,54,132,109,46,226,208,254,32,223,203,121,38,163,125,123,103,34,195,227,136,174,46,53,15,16,92,106,30,54,209,180,168,62,101,253,158,62,62,90,125,146,194,70,191,43,63,151,11,69,151,98,235,26,5,80,74,72,68,168,85,39,73,89,89,154,60,57,0,145,146,191,221,151,198,175,130,191,10,191,104,207,133,94,56,248,35,241,187,192,250,39,196,127,133,127,17,244,73,116,15,24,248,59,196,17,76,246,26,165,139,77,13,229,173,196,23,86,147,69,117,163,107,118,90,157,165,141,246,153,169,216,207,109,169,105,90,150,155,105,169,105,183,118,151,246,150,215,49,127,21,223,183,47,252,16,175,246,155,253,137,238,175,190,40,126,198,183,126,60,253,170,63,103,59,33,224,139,9,126,28,173,157,239,139,191,107,15,10,234,122,159,219,116,109,126,254,231,194,254,0,240,77,173,167,197,159,5,71,168,174,150,235,125,161,219,91,107,118,17,120,170,56,239,188,61,46,145,160,106,190,43,151,135,31,129,246,247,156,85,219,221,23,25,37,163,62,241,240,55,237,19,20,81,218,111,188,8,242,249,133,135,158,24,182,219,121,11,240,28,227,247,198,110,135,220,124,187,77,107,120,199,246,138,130,88,164,70,189,243,10,218,171,236,243,182,124,158,108,203,33,225,193,99,228,153,48,57,207,97,154,254,97,252,25,251,114,233,114,220,104,208,127,108,66,146,61,174,231,133,166,88,217,101,54,50,75,40,116,44,10,201,230,6,45,144,14,226,114,51,154,217,241,103,237,199,164,193,127,28,50,107,54,225,100,177,141,138,52,232,67,163,79,114,173,185,89,134,84,133,32,251,3,248,124,247,246,108,253,167,194,214,189,141,121,174,183,208,250,139,227,70,167,174,120,91,79,187,248,111,224,111,29,199,225,95,129,119,38,123,171,127,12,105,211,95,233,222,53,248,111,164,219,32,130,239,225,47,193,239,18,105,178,134,240,183,194,189,70,75,184,19,79,242,29,117,63,2,89,105,186,134,131,224,233,109,116,221,79,194,143,240,199,209,63,224,145,159,240,79,159,248,120,87,237,81,163,120,163,198,94,10,189,210,255,0,98,207,217,75,83,240,230,191,174,52,30,11,240,109,223,195,79,137,95,16,60,35,168,120,95,84,240,39,236,188,250,95,137,68,246,183,30,27,147,68,146,61,75,197,54,118,186,110,167,13,167,134,116,251,109,2,246,29,22,79,22,104,122,164,122,63,176,135,252,18,131,246,186,255,0,130,147,106,126,25,248,161,241,14,63,16,254,205,191,178,61,198,181,225,125,67,91,241,143,140,244,125,115,65,248,171,241,107,193,58,231,134,37,241,116,90,143,236,231,225,79,17,248,102,75,109,95,77,188,180,212,124,62,144,248,191,85,41,225,200,19,197,208,234,26,36,62,46,184,209,181,125,10,47,237,147,246,62,253,143,190,5,126,195,31,2,188,47,251,62,126,207,190,23,125,3,193,186,3,77,169,235,58,206,169,52,26,143,141,126,34,248,215,81,130,210,31,17,124,72,248,145,226,40,109,32,62,37,241,198,166,214,54,130,226,224,67,5,173,173,173,141,158,147,164,217,233,218,38,157,166,233,150,127,65,130,193,206,60,179,173,167,46,169,90,205,190,239,187,245,255,0,51,41,73,61,22,199,211,244,81,69,122,196,5,20,81,64,31,197,31,252,30,23,255,0,56,237,255,0,187,183,255,0,223,100,175,137,255,0,224,210,255,0,249,72,215,198,175,251,50,127,136,191,250,189,127,103,26,40,172,31,241,126,127,164,64,255,0,67,74,40,162,183,0,162,138,40,3,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 3075; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

