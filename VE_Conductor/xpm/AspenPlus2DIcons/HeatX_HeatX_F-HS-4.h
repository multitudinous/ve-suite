#ifndef GETVESUITE_HeatX_HeatX_F-HS-4_H
#define GETVESUITE_HeatX_HeatX_F-HS-4_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_HeatX_HeatX_F-HS-4( void )
{
    unsigned char osgData[ 5515 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,53,0,142,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,250,35,246,161,255,0,130,204,254,200,95,183,167,195,255,0,218,151,199,223,12,116,127,141,95,179,212,159,17,255,0,224,146,255,0,182,199,236,185,103,225,63,141,95,14,237,52,255,0,20,254,213,95,20,62,60,233,95,15,181,223,217,18,29,46,231,224,23,137,60,113,163,221,120,119,192,255,0,217,191,180,93,180,55,159,16,53,127,13,47,134,46,127,106,107,166,240,220,119,86,190,35,241,181,214,153,247,68,95,240,117,255,0,236,69,54,171,123,161,199,251,34,255,0,193,67,27,85,211,180,253,51,85,188,180,255,0,132,7,246,84,13,6,159,172,220,234,246,154,101,201,145,191,108,16,140,178,220,104,58,178,128,172,89,126,196,197,194,134,66,223,137,191,178,15,236,186,124,65,251,38,126,203,218,247,246,36,147,127,109,254,206,223,5,53,127,52,90,202,194,65,169,124,53,240,205,224,144,48,139,230,4,77,156,228,245,234,105,124,51,251,46,153,255,0,107,47,141,122,8,209,36,63,217,159,179,191,236,193,171,152,190,203,41,41,253,183,241,39,246,188,179,18,109,17,101,67,255,0,194,62,70,113,131,228,245,226,188,69,153,194,21,113,62,234,230,134,250,189,121,100,163,242,209,253,230,142,42,209,215,115,251,127,253,135,63,110,47,217,223,254,10,31,251,57,248,47,246,156,253,153,252,91,47,136,252,7,226,200,150,207,87,208,245,155,120,52,159,31,124,50,241,165,190,157,166,234,90,231,195,63,138,30,23,134,242,224,248,91,199,122,117,182,175,166,75,44,43,61,205,141,253,134,173,167,235,186,29,254,173,225,221,91,72,213,239,254,186,175,243,17,255,0,130,118,95,254,214,127,178,215,129,254,29,252,83,253,143,126,46,120,139,224,183,138,190,43,126,206,127,9,173,62,32,207,162,104,190,18,241,102,129,227,61,18,31,14,248,127,90,240,247,252,36,158,8,248,129,225,109,111,69,212,245,221,50,238,239,80,109,43,85,151,78,58,166,149,7,138,245,171,77,50,246,206,207,93,214,32,191,253,54,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,191,255,0,67,173,118,188,199,13,22,227,38,211,139,179,254,174,46,70,127,119,20,87,240,143,255,0,13,251,255,0,5,172,255,0,163,209,215,63,241,31,127,101,111,254,135,90,63,225,191,127,224,181,159,244,122,58,231,254,35,239,236,173,255,0,208,235,75,251,75,11,252,207,238,255,0,131,230,28,146,63,187,138,43,248,71,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,183,255,0,67,173,31,240,223,191,240,90,207,250,61,29,115,255,0,17,247,246,86,255,0,232,117,163,251,75,11,252,207,238,255,0,131,230,28,146,63,187,138,43,248,71,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,183,255,0,67,173,31,240,223,191,240,90,207,250,61,29,115,255,0,17,247,246,86,255,0,232,117,163,251,75,11,252,207,238,255,0,131,230,28,146,63,187,138,43,248,71,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,183,255,0,67,173,31,240,223,191,240,90,207,250,61,29,115,255,0,17,247,246,86,255,0,232,117,163,251,75,11,252,207,238,255,0,131,230,28,146,63,187,138,43,248,71,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,183,255,0,67,173,31,240,223,191,240,90,207,250,61,29,115,255,0,17,247,246,86,255,0,232,117,163,251,75,11,252,207,238,14,73,31,213,183,136,127,224,161,126,13,240,167,199,175,11,126,204,122,247,192,127,218,6,195,227,127,141,62,62,120,43,224,255,0,134,188,23,229,252,9,186,26,143,195,143,136,158,16,253,167,188,121,224,207,218,192,248,146,207,227,180,154,77,159,192,27,223,9,254,198,159,180,165,216,211,103,191,139,226,157,191,252,43,83,5,223,195,75,123,173,99,195,240,106,191,160,85,254,127,126,33,253,160,191,224,163,30,42,241,159,133,254,49,235,255,0,19,52,91,255,0,218,87,193,158,51,240,71,137,252,53,251,76,127,194,179,240,117,175,196,109,55,64,248,115,240,207,246,160,248,87,224,223,134,159,240,131,217,120,86,63,135,87,158,9,179,208,255,0,108,207,218,82,231,237,23,30,6,151,196,151,23,255,0,18,55,221,120,130,123,93,31,64,180,210,240,254,41,127,193,66,127,224,180,54,222,56,253,155,224,185,253,183,60,113,109,62,163,241,163,93,179,211,225,210,126,19,126,207,190,30,211,239,238,211,246,120,248,243,168,61,167,138,52,157,3,224,133,173,175,142,180,49,167,216,223,79,30,149,173,193,169,105,144,234,118,58,126,183,21,146,107,58,62,145,168,216,16,204,48,210,114,92,251,107,183,68,174,250,250,139,146,71,250,16,81,95,201,55,236,253,255,0,7,14,126,208,94,14,248,143,161,104,191,183,135,192,95,134,169,240,155,197,90,214,147,166,95,252,84,253,157,52,79,136,58,14,169,240,115,75,123,63,16,127,104,120,171,196,223,14,188,81,226,239,23,207,241,103,69,125,69,52,38,184,183,209,117,13,39,88,210,180,237,55,82,186,177,210,252,87,127,113,99,164,143,234,35,224,175,198,175,133,95,180,103,194,175,3,252,110,248,35,227,141,19,226,63,194,191,136,250,36,94,32,240,119,140,116,9,102,107,29,82,193,230,154,210,234,222,226,214,242,24,174,180,109,110,207,83,181,190,177,212,244,203,232,45,181,45,43,82,211,110,244,221,74,210,210,254,210,230,218,46,186,117,105,213,87,167,37,36,38,154,220,252,15,255,0,130,111,252,9,109,95,254,9,223,251,5,106,203,167,135,93,79,246,47,253,150,245,16,248,140,239,23,191,3,188,11,115,184,146,122,159,55,63,141,30,6,248,18,242,255,0,193,68,63,106,61,39,236,3,54,95,177,127,236,23,168,148,219,30,7,246,167,199,15,248,41,5,168,99,207,127,236,124,127,192,57,175,171,127,224,151,191,19,124,63,167,255,0,193,51,255,0,224,157,214,19,11,99,53,151,236,47,251,36,218,74,90,54,45,230,219,252,1,248,127,12,153,59,121,59,208,215,9,225,221,124,234,159,240,86,159,142,126,61,211,254,40,248,207,194,222,26,240,151,236,47,251,13,89,248,135,225,110,131,165,124,51,185,240,31,197,217,117,127,140,223,240,83,219,45,44,252,66,212,60,79,240,227,82,241,54,150,116,11,187,164,212,116,159,248,68,252,69,225,127,58,242,229,147,93,254,219,211,214,59,8,255,0,61,231,155,196,102,42,79,145,46,107,59,55,111,222,195,183,127,215,93,14,180,151,45,55,107,189,63,35,242,83,254,9,113,251,58,195,227,159,217,155,246,80,191,147,79,89,86,235,246,53,248,29,169,23,146,4,37,228,187,248,121,240,217,139,224,71,32,41,229,178,97,182,134,57,59,143,56,63,169,39,246,58,178,206,14,147,22,114,120,251,42,14,157,113,254,129,95,35,255,0,193,36,60,64,214,127,179,23,129,103,135,226,151,140,188,59,105,241,171,246,54,248,75,169,124,55,208,52,221,47,225,77,214,141,251,30,91,107,95,15,180,39,211,188,37,251,57,92,120,167,225,214,161,127,226,141,11,66,139,196,154,28,26,99,252,91,191,248,165,170,75,23,195,189,25,181,173,67,86,154,227,196,51,107,255,0,166,218,210,120,155,83,248,55,225,63,134,54,95,182,55,199,223,14,120,219,195,154,253,198,179,172,126,209,90,55,132,191,99,41,254,50,120,247,78,158,127,18,205,23,132,252,89,161,248,131,246,86,190,248,125,103,160,67,30,187,165,197,28,154,23,129,52,93,84,167,130,244,211,54,165,44,178,235,18,106,215,140,107,235,85,151,214,57,125,231,167,44,157,181,242,93,54,252,175,184,160,189,197,120,221,255,0,195,30,3,255,0,12,117,101,255,0,64,152,191,240,21,127,249,2,143,248,99,155,47,250,4,69,255,0,128,171,255,0,200,21,245,238,179,226,249,53,63,140,126,20,248,157,99,241,195,199,222,28,240,79,135,116,11,173,27,87,253,157,116,109,19,224,36,255,0,7,60,123,168,220,65,226,104,98,241,103,139,53,191,16,124,33,191,248,131,101,226,8,95,93,210,229,142,61,15,199,122,46,150,95,193,154,96,155,76,150,41,117,136,245,108,13,19,86,213,116,175,248,94,127,111,253,166,254,47,120,155,254,22,208,213,255,0,225,1,254,219,240,231,236,183,7,252,51,7,246,151,252,37,63,98,31,3,63,225,28,253,159,52,255,0,237,255,0,236,241,175,233,63,99,255,0,133,151,255,0,11,19,127,252,32,250,95,219,254,219,231,107,95,219,28,169,173,63,218,127,242,89,119,94,94,143,242,42,203,249,63,173,63,175,248,115,230,31,248,99,171,47,250,4,197,255,0,128,171,255,0,200,20,127,195,29,89,127,208,38,47,252,5,95,254,64,175,126,214,83,196,186,159,193,207,10,124,49,177,253,177,254,62,248,115,198,190,28,215,238,53,157,95,246,138,209,188,37,251,25,79,241,147,199,186,116,211,248,150,88,188,39,226,205,15,196,31,178,181,255,0,195,235,63,15,195,30,187,165,197,28,154,23,129,116,109,84,167,131,52,195,46,165,44,146,235,18,106,221,254,179,226,249,53,63,140,126,19,248,157,101,241,195,199,222,29,240,79,135,52,11,173,27,87,253,157,52,109,19,224,36,255,0,7,60,123,168,207,7,137,161,139,197,158,44,214,252,65,240,134,255,0,226,13,151,136,33,125,119,74,150,56,244,47,29,232,186,89,127,6,105,130,109,50,88,165,214,35,213,139,173,63,218,127,242,89,116,181,190,254,159,141,130,203,249,63,173,63,175,248,115,228,47,248,99,155,47,250,4,69,255,0,128,171,255,0,200,20,127,195,29,89,127,208,38,47,252,5,95,254,64,175,167,180,77,91,85,210,191,225,121,253,191,246,155,248,189,226,111,248,91,67,87,255,0,132,7,251,111,195,159,178,220,31,240,204,31,218,95,240,148,253,136,124,12,255,0,132,115,246,124,211,255,0,183,255,0,179,198,191,164,253,143,254,22,95,252,44,77,255,0,240,131,233,127,111,251,111,157,173,127,108,115,250,202,120,151,83,248,55,225,63,134,22,63,182,63,199,223,14,120,215,195,154,253,206,179,171,254,209,90,55,132,191,99,41,254,50,120,247,78,158,127,18,203,23,132,252,89,161,248,131,246,87,191,248,125,103,225,248,99,215,116,184,163,147,66,240,38,139,170,148,240,102,154,102,212,165,150,93,98,77,89,166,174,151,214,127,242,89,121,121,122,126,157,194,203,249,63,173,63,175,248,115,192,127,225,142,172,191,232,19,23,254,2,175,255,0,32,81,255,0,12,115,101,255,0,64,136,191,240,21,127,249,2,190,189,214,124,95,38,167,241,143,194,159,19,172,126,56,120,251,195,158,9,240,238,129,117,163,106,255,0,179,174,141,162,124,4,159,224,231,143,117,27,136,60,77,12,94,44,241,102,183,226,15,132,55,255,0,16,108,188,65,11,235,186,92,177,199,161,248,239,69,210,203,248,51,76,19,105,146,197,46,177,30,173,129,162,106,218,174,149,255,0,11,207,237,255,0,180,223,197,239,19,127,194,218,26,191,252,32,63,219,126,28,253,150,224,255,0,134,96,254,210,255,0,132,167,236,67,224,103,252,35,159,179,230,159,253,191,253,158,53,253,39,236,127,240,178,255,0,225,98,111,255,0,132,31,75,251,127,219,124,237,107,251,97,38,180,255,0,105,255,0,201,101,221,121,122,63,200,44,191,147,250,211,250,255,0,135,62,97,255,0,134,58,178,255,0,160,76,95,248,10,191,252,129,71,252,49,213,151,253,2,34,255,0,192,85,255,0,228,10,247,237,101,60,75,169,252,28,240,167,195,27,31,219,31,227,239,135,60,107,225,205,126,227,89,213,255,0,104,173,27,194,95,177,148,255,0,25,60,123,167,77,63,137,101,139,194,126,44,208,252,65,251,43,95,252,62,179,240,252,49,235,186,92,81,201,161,120,23,70,213,74,120,51,76,50,234,82,201,46,177,38,173,222,235,126,45,147,85,248,197,225,95,137,182,31,28,126,32,120,103,193,94,30,240,253,206,141,171,254,206,250,38,137,240,10,127,131,190,60,212,102,135,196,177,67,226,207,21,235,126,35,248,63,168,124,64,179,241,4,50,107,186,92,177,197,161,248,235,70,210,139,248,51,77,19,105,178,199,46,177,30,170,156,146,77,172,69,218,191,217,150,182,181,186,117,211,245,176,210,95,203,253,105,253,127,195,159,34,127,195,29,89,127,208,34,47,252,5,95,254,64,175,142,255,0,105,207,217,94,203,70,248,199,255,0,4,250,183,254,202,64,186,239,237,121,227,141,42,68,91,104,129,153,109,127,96,47,219,143,196,70,33,190,209,84,156,232,42,64,39,25,64,78,49,185,127,118,63,225,121,248,79,254,125,180,99,255,0,0,182,255,0,228,138,248,119,246,179,248,181,225,141,99,227,191,252,19,40,36,58,108,113,233,223,182,231,196,93,70,229,160,88,85,140,11,255,0,4,209,255,0,130,135,89,97,154,57,207,201,231,94,193,247,134,204,227,126,87,32,188,45,90,206,172,155,90,42,117,63,244,220,191,224,107,230,41,69,91,69,213,126,104,252,157,253,170,63,101,125,31,79,210,53,24,102,211,147,27,5,185,89,206,197,133,19,200,37,149,124,240,241,109,153,67,71,34,153,138,155,195,36,96,48,101,175,72,255,0,131,120,255,0,104,31,136,254,14,253,160,126,60,254,193,250,214,187,172,248,175,225,50,252,53,215,63,104,191,133,118,26,150,173,104,250,95,193,205,83,64,248,131,225,15,11,124,69,240,207,133,116,255,0,248,71,197,196,186,39,139,47,254,47,120,123,91,184,183,23,214,218,126,149,172,120,79,84,190,181,211,103,190,241,94,173,122,191,87,126,215,30,52,240,251,88,234,198,21,130,49,49,150,40,254,200,246,251,174,28,164,241,40,218,254,81,124,60,219,84,128,225,150,6,218,126,93,167,249,144,185,253,135,62,44,255,0,193,80,255,0,105,157,99,246,117,248,3,226,31,135,126,16,241,183,132,254,31,120,139,227,62,163,170,124,96,213,188,75,225,255,0,11,79,225,127,13,120,163,194,94,14,190,176,178,188,240,95,132,188,65,119,38,190,250,159,197,45,2,72,98,146,198,59,118,130,206,241,158,234,57,35,130,43,143,123,35,175,89,205,39,119,221,92,202,172,85,228,175,109,79,214,255,0,216,51,246,150,176,208,127,97,175,216,199,67,123,185,85,244,111,217,67,246,118,210,157,67,69,133,109,63,225,15,131,237,24,12,206,56,204,39,181,47,131,255,0,105,109,62,15,219,147,246,137,215,13,220,219,53,47,217,71,246,50,210,131,102,44,150,209,190,47,126,222,87,110,51,246,142,64,26,234,116,201,249,143,3,140,255,0,56,95,178,79,237,59,241,43,197,95,179,110,159,39,194,207,134,63,18,62,38,248,75,246,98,248,31,240,249,254,55,120,171,225,231,132,188,85,227,47,12,252,29,240,230,131,224,59,166,127,16,124,82,215,60,55,161,93,90,252,61,208,198,153,224,143,21,220,253,171,86,154,210,220,91,248,99,80,155,205,49,217,92,188,126,65,164,127,193,72,188,25,105,241,163,199,222,53,111,17,233,2,215,196,31,12,62,17,120,90,41,191,183,173,68,114,79,224,239,21,124,110,213,174,35,73,138,109,145,210,63,28,219,22,65,202,137,209,137,195,138,213,229,147,115,197,190,79,226,95,167,247,227,47,210,228,243,252,22,118,177,251,27,251,22,254,213,182,127,13,255,0,103,47,217,138,210,227,84,130,209,236,191,101,255,0,131,90,22,26,229,99,101,107,31,135,158,8,77,164,155,132,216,12,54,240,112,31,159,47,230,64,195,35,235,255,0,248,120,6,153,255,0,67,13,191,254,12,83,255,0,150,53,193,255,0,193,1,191,224,141,58,191,237,85,240,95,192,255,0,180,199,237,231,240,222,250,207,246,100,213,254,2,120,55,194,63,179,159,193,237,87,196,127,20,254,28,124,69,248,143,168,45,151,131,39,95,218,58,243,84,248,117,227,125,6,255,0,194,223,12,198,139,161,234,154,111,133,236,239,205,227,120,198,219,197,179,248,154,27,77,55,64,211,252,39,170,248,175,250,42,255,0,135,3,255,0,193,39,191,232,217,124,69,255,0,137,57,251,91,255,0,243,247,174,201,228,202,172,229,82,77,39,38,223,94,247,252,73,83,181,183,211,254,1,248,35,255,0,15,0,211,63,232,97,183,255,0,193,130,127,242,198,143,248,120,6,153,255,0,67,13,191,254,12,19,255,0,150,53,251,221,255,0,14,7,255,0,130,79,127,209,178,248,139,255,0,18,115,246,183,255,0,231,239,71,252,56,31,254,9,61,255,0,70,203,226,47,252,73,207,218,223,255,0,159,189,71,246,20,63,153,126,62,95,228,63,105,234,126,8,255,0,195,192,52,207,250,24,109,255,0,240,96,159,252,177,163,254,30,1,166,127,208,195,111,255,0,131,4,255,0,229,141,126,247,127,195,129,255,0,224,147,223,244,108,190,34,255,0,196,156,253,173,255,0,249,251,209,255,0,14,7,255,0,130,79,127,209,178,248,139,255,0,18,115,246,183,255,0,231,239,71,246,20,63,153,126,62,95,228,30,211,212,252,17,255,0,135,128,105,159,244,48,219,255,0,224,193,63,249,99,71,252,60,3,76,255,0,161,134,223,255,0,6,9,255,0,203,26,253,238,255,0,135,3,255,0,193,39,191,232,217,124,69,255,0,137,57,251,91,255,0,243,247,163,254,28,15,255,0,4,158,255,0,163,101,241,23,254,36,231,237,111,255,0,207,222,143,236,40,127,50,252,124,191,200,61,167,169,248,35,255,0,15,0,211,63,232,97,183,255,0,193,130,127,242,198,143,248,120,6,153,255,0,67,13,191,254,12,19,255,0,150,53,251,221,255,0,14,7,255,0,130,79,127,209,178,248,139,255,0,18,115,246,183,255,0,231,239,71,252,56,31,254,9,61,255,0,70,203,226,47,252,73,207,218,223,255,0,159,189,31,216,80,254,101,248,249,127,144,123,79,83,240,71,254,30,1,166,127,208,195,111,255,0,131,4,255,0,229,141,31,240,240,13,51,254,134,27,127,252,24,39,255,0,44,107,247,187,254,28,15,255,0,4,158,255,0,163,101,241,23,254,36,231,237,111,255,0,207,222,143,248,112,63,252,18,123,254,141,151,196,95,248,147,159,181,191,255,0,63,122,63,176,161,252,203,79,95,47,242,15,105,234,126,8,255,0,195,192,52,207,250,24,109,253,63,228,32,157,125,63,228,35,94,23,241,55,246,218,211,252,67,241,159,246,45,187,77,102,27,143,248,69,191,104,239,24,120,129,194,221,172,198,37,159,246,58,253,171,252,49,230,21,19,205,129,187,196,138,185,217,199,153,212,100,87,220,63,16,191,99,127,248,36,255,0,135,191,110,255,0,134,31,2,60,41,240,63,225,30,185,251,50,120,163,227,231,193,31,217,71,198,127,25,71,237,117,251,92,106,75,225,223,218,163,198,191,8,63,224,168,62,47,248,147,251,51,31,136,58,111,237,100,158,31,210,191,104,29,39,199,255,0,179,31,236,89,166,15,6,221,218,63,136,244,255,0,248,104,51,101,127,166,205,123,226,223,9,203,167,125,211,251,70,127,193,184,31,176,215,196,109,87,246,121,95,129,126,20,241,127,192,43,79,7,252,110,212,188,89,241,155,198,158,29,248,247,241,139,196,223,16,111,190,20,201,251,63,252,119,240,133,159,135,62,27,233,223,26,175,188,117,225,197,241,36,255,0,24,124,91,240,146,226,242,91,237,38,213,215,195,218,86,182,45,53,40,175,26,11,91,202,167,146,70,14,77,77,94,210,143,95,181,27,126,191,211,15,105,234,191,164,126,6,252,122,253,180,110,60,101,54,151,225,31,10,157,71,198,62,49,241,174,171,162,248,103,194,62,17,240,229,141,238,191,226,79,19,120,163,95,187,77,43,66,240,255,0,135,244,77,22,73,174,252,77,174,221,94,66,32,180,211,45,146,226,242,250,125,65,45,109,160,47,34,239,254,137,255,0,224,133,95,240,79,127,16,254,205,95,9,124,65,251,83,124,121,240,231,137,124,59,251,77,126,211,154,101,163,220,120,7,199,190,26,240,182,153,226,47,128,63,8,180,237,123,86,191,240,223,129,44,229,178,142,109,79,70,241,15,138,85,180,63,19,248,190,198,234,230,198,72,110,45,188,57,225,253,83,66,178,213,252,31,61,213,231,222,127,179,71,252,18,227,246,8,253,144,188,101,47,196,95,128,159,179,159,134,188,55,241,4,108,254,203,241,215,139,60,73,227,255,0,139,190,50,240,159,252,74,181,237,10,247,254,21,255,0,137,254,49,248,183,95,189,248,113,246,253,19,196,218,197,158,171,253,131,62,155,253,177,107,60,118,250,167,219,33,182,181,72,126,252,174,252,30,95,79,8,180,247,164,136,148,185,180,181,143,231,251,254,11,89,24,149,126,49,33,56,199,252,16,31,254,11,219,38,113,159,245,51,127,193,56,37,219,140,247,217,143,108,231,154,254,128,104,162,187,236,174,223,87,250,18,20,81,69,48,10,40,162,128,10,40,162,128,10,40,162,128,10,40,162,128,10,40,162,128,10,40,162,128,10,40,162,128,63,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 5515; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

