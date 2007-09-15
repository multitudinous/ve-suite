#ifndef GETVESUITE_HeatX_HeatX_G-HT-2_H
#define GETVESUITE_HeatX_HeatX_G-HT-2_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_HeatX_HeatX_G-HT-2( void )
{
    unsigned char osgData[ 5505 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,53,0,142,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,250,43,246,160,255,0,130,205,126,200,127,183,159,195,255,0,218,155,199,191,12,116,143,141,127,179,204,159,18,127,224,146,223,182,199,236,185,105,225,63,141,95,14,172,244,255,0,20,126,213,127,20,62,60,105,127,15,117,255,0,217,18,13,50,231,224,23,137,124,111,163,220,248,123,192,231,76,253,162,109,161,189,248,129,171,120,105,124,49,115,251,82,220,159,13,199,117,107,226,63,27,221,105,159,115,71,255,0,7,95,254,196,83,106,215,186,28,127,178,47,252,20,49,181,93,59,78,211,53,107,219,63,248,64,63,101,80,208,105,250,205,214,175,103,166,92,180,141,251,96,4,101,150,231,65,213,144,42,177,117,54,68,186,168,100,45,248,159,251,33,254,203,167,196,31,178,111,236,189,175,13,14,89,191,183,63,103,127,130,154,191,154,45,100,113,47,246,159,195,111,13,94,121,129,132,71,112,62,121,57,7,7,52,158,26,253,151,12,223,181,151,198,189,4,232,114,159,236,239,217,223,246,95,213,204,95,101,147,40,53,191,137,63,181,229,152,125,190,87,1,135,135,176,56,231,202,239,131,94,39,246,156,99,83,18,249,85,224,245,215,179,140,116,251,219,52,80,186,91,235,255,0,14,127,112,95,176,255,0,237,189,251,61,255,0,193,66,191,103,95,5,254,211,95,179,95,138,174,53,255,0,2,248,178,8,109,181,125,7,92,181,135,71,241,255,0,195,63,24,166,151,165,234,218,199,195,111,138,30,22,142,242,115,225,127,28,88,89,107,58,76,239,18,207,115,99,168,233,218,198,157,175,104,87,250,183,135,53,125,31,88,191,250,222,191,204,111,254,9,231,125,251,89,126,203,158,4,248,105,241,83,246,60,248,195,226,159,131,30,41,248,173,251,55,252,38,178,241,235,105,90,39,133,60,89,225,255,0,23,104,48,120,115,195,186,238,134,218,255,0,130,188,127,225,109,115,68,212,181,93,59,87,191,215,31,72,213,36,211,142,171,165,67,226,141,110,219,78,189,179,181,215,181,155,125,67,244,179,254,27,247,254,11,89,255,0,71,163,174,127,226,62,254,202,223,253,14,181,219,253,161,134,77,198,82,106,81,118,122,18,160,223,204,254,238,40,175,225,31,254,27,247,254,11,89,255,0,71,163,174,127,226,62,254,202,223,253,14,180,127,195,126,255,0,193,107,63,232,244,117,207,252,71,223,217,91,255,0,161,214,151,246,150,18,246,231,119,244,244,255,0,49,242,72,254,238,40,175,225,31,254,27,247,254,11,89,255,0,71,163,174,127,226,62,254,202,223,253,14,180,127,195,126,255,0,193,107,63,232,244,117,207,252,71,223,217,91,255,0,161,214,143,237,44,47,243,191,187,211,252,195,146,71,247,49,226,205,103,81,240,231,133,124,75,226,29,31,194,126,32,241,238,175,161,120,127,89,214,116,191,2,248,78,231,194,182,126,42,241,166,163,165,233,215,55,214,62,19,240,213,231,142,188,75,162,232,150,190,32,212,110,96,138,206,206,77,99,88,210,116,164,185,188,141,181,13,78,194,208,77,117,23,203,159,178,119,237,179,240,175,246,209,159,226,150,167,240,71,195,223,16,175,126,25,124,55,111,130,41,163,252,101,241,46,149,225,175,15,248,27,226,195,124,118,253,157,126,22,126,212,190,28,31,14,244,25,188,91,39,139,236,127,179,190,17,124,110,248,79,117,171,255,0,194,85,225,127,12,132,188,241,156,118,58,111,246,156,246,26,176,211,255,0,144,175,248,111,223,248,45,103,253,30,142,185,255,0,136,251,251,43,127,244,58,215,153,252,55,253,160,127,224,163,31,0,127,225,39,209,191,101,143,137,154,55,236,195,240,219,196,173,240,185,255,0,225,82,124,59,248,103,224,239,30,248,59,68,111,132,31,179,199,193,127,217,115,193,135,71,215,127,105,15,10,248,235,197,77,179,225,7,192,15,134,86,119,31,109,241,29,224,158,235,73,158,249,177,115,121,112,242,47,237,44,55,50,247,223,45,187,117,210,223,215,152,185,36,127,160,37,21,252,35,255,0,195,126,255,0,193,107,63,232,244,117,207,252,71,223,217,91,255,0,161,214,143,248,111,223,248,45,103,253,30,142,185,255,0,136,251,251,43,127,244,58,211,254,210,194,255,0,59,251,189,63,204,124,146,63,187,138,43,248,71,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,183,255,0,67,173,31,240,223,191,240,90,207,250,61,29,115,255,0,17,247,246,86,255,0,232,117,167,253,163,133,254,119,247,127,193,243,14,73,31,221,197,21,252,35,255,0,195,126,255,0,193,107,63,232,244,117,207,252,71,223,217,91,255,0,161,214,188,99,226,143,252,20,39,254,11,65,107,227,143,217,190,11,159,219,115,199,54,211,234,63,25,245,219,61,58,29,35,225,63,236,249,225,237,63,80,188,143,246,119,248,245,168,73,105,226,141,39,65,248,31,107,105,227,157,9,108,44,111,174,35,210,181,184,53,45,50,45,74,199,78,214,225,178,77,103,71,210,53,27,7,28,195,13,39,101,55,123,55,183,101,119,248,11,145,159,232,63,69,127,36,223,179,247,252,28,57,251,65,120,63,226,62,133,162,126,222,31,1,190,26,167,194,111,21,235,58,78,155,127,241,83,246,116,209,62,32,232,58,167,193,205,45,236,252,65,253,163,226,175,19,124,58,241,79,139,188,93,63,197,157,21,245,36,208,77,197,190,137,168,105,58,198,149,167,105,218,149,213,142,149,226,187,233,236,116,161,253,68,124,21,248,213,240,171,246,140,248,85,224,127,141,223,4,124,113,162,124,71,248,87,241,31,68,139,196,30,14,241,142,129,44,205,99,170,88,60,211,90,93,91,220,90,222,67,21,214,141,173,217,234,118,183,214,58,158,153,125,5,182,165,165,106,90,109,222,155,169,90,90,95,218,92,219,69,211,78,181,58,203,154,156,148,144,154,107,115,240,67,254,9,191,240,37,245,127,248,39,135,236,23,171,125,128,56,212,255,0,98,255,0,217,115,81,15,182,51,188,94,252,15,240,53,200,108,150,238,36,7,241,164,240,47,192,167,147,254,10,33,251,81,233,63,96,4,217,126,197,255,0,176,86,162,83,108,124,13,83,227,143,252,20,130,216,54,51,142,78,143,143,248,7,210,190,173,255,0,130,94,252,77,208,44,63,224,154,31,240,78,235,25,133,177,154,203,246,22,253,146,45,37,44,132,159,50,219,224,15,195,248,95,113,219,203,110,67,159,165,31,15,254,38,248,125,63,224,165,255,0,181,189,249,91,111,38,231,246,23,255,0,130,119,90,71,242,54,223,50,203,227,247,252,21,14,105,112,161,122,237,191,135,63,65,95,157,206,117,125,182,101,101,162,191,254,158,167,250,29,145,75,150,30,105,126,71,227,239,252,19,47,246,114,183,241,175,236,165,251,32,234,18,216,67,114,111,191,99,31,128,58,171,75,228,36,140,94,235,225,159,195,199,85,144,136,92,29,169,34,227,42,24,25,9,39,230,57,253,37,255,0,134,58,178,255,0,160,68,95,248,10,191,252,129,94,113,255,0,4,140,248,151,225,191,14,254,197,63,177,44,87,86,90,112,185,143,246,25,253,155,109,100,146,125,141,189,163,248,79,240,213,25,216,60,192,44,134,75,119,39,184,36,240,43,245,111,254,23,159,132,255,0,231,219,70,255,0,190,45,191,249,34,163,27,90,178,196,214,81,189,148,159,230,133,8,199,150,61,116,63,58,199,236,117,102,127,230,17,23,183,250,42,143,231,97,219,138,95,248,99,155,46,127,226,81,31,25,235,104,163,167,95,249,112,250,126,117,245,206,173,226,217,117,31,140,94,19,248,155,103,241,207,226,14,131,224,175,14,104,23,58,62,175,251,59,233,90,31,192,25,190,14,248,243,80,158,223,196,176,197,226,175,21,235,122,231,193,235,223,136,22,122,252,50,107,186,100,177,199,161,248,235,69,210,218,79,6,233,162,109,54,104,165,214,34,213,112,244,77,91,84,210,127,225,121,253,187,246,155,248,189,226,95,248,91,95,218,223,240,128,255,0,109,248,111,246,91,183,255,0,134,96,254,209,255,0,132,167,236,95,240,163,63,225,28,253,159,52,255,0,248,72,63,179,255,0,225,32,210,190,199,255,0,11,47,254,22,30,255,0,248,65,244,191,183,253,183,206,214,191,182,51,132,219,81,111,17,200,218,90,114,201,246,210,246,254,187,142,203,249,63,173,63,175,248,115,230,31,248,99,171,46,191,217,17,1,234,109,80,125,58,216,250,254,52,127,195,29,89,116,254,201,132,31,123,85,252,58,216,122,127,250,235,223,181,148,241,46,169,240,111,194,127,12,108,191,108,127,143,190,28,241,183,135,117,251,157,99,88,253,162,180,111,9,126,198,83,252,100,241,238,157,60,254,37,150,47,9,248,179,67,241,7,236,175,127,240,250,207,64,134,61,119,75,138,41,116,47,2,104,186,169,79,5,233,134,93,78,89,101,214,36,213,187,253,103,197,239,170,124,100,240,159,196,251,47,141,254,62,240,231,130,124,57,160,92,232,218,199,236,235,163,104,159,1,39,248,55,227,221,70,226,15,18,197,31,139,60,89,173,248,131,225,13,255,0,196,27,61,126,25,53,221,46,88,162,208,188,119,162,233,69,252,23,166,9,116,201,98,151,88,143,86,106,91,127,180,255,0,228,147,254,239,151,94,159,141,130,203,249,63,173,63,175,248,115,228,63,248,99,155,46,127,226,81,31,25,235,104,163,167,95,249,112,250,126,116,159,240,199,86,93,127,178,34,3,212,218,160,250,117,177,245,252,107,233,237,19,86,213,52,159,248,94,127,110,253,166,254,47,120,151,254,22,215,246,183,252,32,63,219,126,27,253,150,237,255,0,225,152,63,180,127,225,41,251,23,252,40,207,248,71,63,103,205,63,254,18,15,236,255,0,248,72,52,175,177,255,0,194,203,255,0,133,135,191,254,16,125,47,237,255,0,109,243,181,175,237,142,127,89,79,18,234,159,6,252,39,240,198,203,246,199,248,251,225,207,27,120,119,95,185,214,53,143,218,43,70,240,151,236,101,63,198,79,30,233,211,207,226,89,98,240,159,139,52,63,16,126,202,247,255,0,15,172,244,8,99,215,116,184,162,151,66,240,38,139,170,148,240,94,152,101,212,229,150,93,98,77,89,166,174,191,218,85,180,251,19,254,239,144,89,127,39,245,167,245,255,0,14,120,15,252,49,213,151,79,236,152,65,247,181,95,195,173,135,167,255,0,174,151,254,24,230,203,159,248,148,71,198,122,218,40,233,215,254,92,62,159,157,125,121,172,248,189,245,79,140,158,19,248,159,101,241,191,199,222,28,240,79,135,52,11,157,27,88,253,157,116,109,19,224,36,255,0,6,252,123,168,220,65,226,88,163,241,103,139,53,191,16,124,33,191,248,131,103,175,195,38,187,165,203,20,90,23,142,244,93,40,191,130,244,193,46,153,44,82,235,17,234,216,26,38,173,170,105,63,240,188,254,221,251,77,252,94,241,47,252,45,175,237,111,248,64,127,182,252,55,251,45,219,255,0,195,48,127,104,255,0,194,83,246,47,248,81,159,240,142,126,207,154,127,252,36,31,217,255,0,240,144,105,95,99,255,0,133,151,255,0,11,15,127,252,32,250,95,219,254,219,231,107,95,219,9,75,111,246,158,223,98,127,221,242,11,47,228,254,180,254,191,225,207,152,127,225,142,172,186,255,0,100,68,7,169,181,65,244,235,99,235,248,208,63,99,155,35,255,0,48,136,135,214,213,127,249,3,233,237,154,247,237,101,60,75,170,124,27,240,159,195,27,47,219,31,227,239,135,60,109,225,221,126,231,88,214,63,104,173,27,194,95,177,148,255,0,25,60,123,167,79,63,137,101,139,194,126,44,208,252,65,251,43,223,252,62,179,208,33,143,93,210,226,138,93,11,192,154,46,170,83,193,122,97,151,83,150,89,117,137,53,110,247,92,241,108,154,183,198,47,10,124,78,176,248,229,241,3,195,62,10,240,238,129,113,163,234,255,0,179,198,135,161,252,1,159,224,239,142,245,25,161,241,44,81,120,179,197,122,223,137,62,15,106,31,16,44,252,65,11,235,186,92,145,197,161,248,235,70,210,139,248,51,77,18,233,146,199,46,177,30,172,57,89,94,56,139,181,211,150,74,239,75,116,210,253,63,27,5,151,242,127,90,127,95,240,231,200,159,240,199,86,95,244,8,139,255,0,1,87,255,0,144,43,227,191,218,119,246,86,179,209,190,49,127,193,62,173,206,148,129,53,223,218,243,199,26,91,162,219,198,166,100,181,253,128,191,110,63,17,152,198,251,69,4,238,208,84,128,199,110,80,19,128,50,191,187,31,240,188,252,39,255,0,62,218,55,253,241,109,255,0,201,21,240,231,237,105,241,107,194,250,191,199,159,248,38,88,72,116,216,227,211,127,109,207,136,218,141,203,192,177,43,121,11,255,0,4,209,255,0,130,135,89,97,154,57,206,19,206,189,132,243,242,110,10,91,35,32,188,45,90,174,163,190,222,206,167,254,155,98,148,99,100,173,187,95,154,63,39,255,0,106,143,217,99,71,176,210,53,24,38,211,144,13,130,6,89,201,68,137,19,200,220,234,162,96,241,149,148,6,142,68,243,136,251,105,146,53,12,29,107,209,255,0,224,222,63,218,7,226,63,131,255,0,104,47,143,63,176,118,181,174,235,94,43,248,76,159,13,117,207,218,47,225,93,142,165,171,90,54,151,240,115,83,208,126,33,120,67,194,223,17,60,51,225,93,63,254,17,245,184,151,69,241,101,255,0,197,223,15,235,87,22,226,254,219,79,210,181,143,9,234,151,214,186,117,197,247,138,245,107,225,245,119,237,113,227,95,15,181,134,172,208,139,120,132,222,108,49,139,71,183,205,195,148,158,37,249,92,196,95,13,48,85,32,54,225,19,149,63,46,218,254,99,238,191,97,191,139,63,240,84,63,218,103,88,253,157,190,0,248,135,225,215,132,60,109,225,63,135,254,34,248,209,168,234,159,24,117,111,18,104,30,22,155,194,222,26,241,63,132,188,29,125,97,101,123,224,207,8,120,134,238,77,125,245,63,138,94,31,146,24,158,198,59,118,183,179,188,103,187,142,72,224,138,227,223,201,43,213,115,74,206,73,173,109,183,77,204,170,69,106,187,119,63,92,127,96,223,218,90,195,65,253,134,255,0,99,45,17,238,230,70,209,191,101,31,217,219,74,117,6,32,21,244,239,132,30,15,179,101,92,220,14,55,67,198,64,224,244,163,193,223,180,181,132,63,183,39,237,19,173,155,185,182,106,63,178,143,236,101,164,171,102,44,151,209,190,47,254,222,87,140,188,92,114,2,235,169,142,122,177,200,28,110,254,112,191,100,159,218,119,226,95,138,191,102,235,9,62,22,124,49,248,145,241,55,194,127,179,23,193,15,135,207,241,187,197,95,15,60,35,226,175,25,120,111,224,239,135,116,47,1,93,51,248,135,226,150,185,225,173,10,234,215,225,238,134,52,207,3,248,174,231,237,90,180,214,182,226,223,195,26,132,222,111,149,101,112,209,249,6,147,255,0,5,34,240,101,159,198,143,31,248,213,252,71,164,45,175,136,62,24,252,33,240,180,51,29,122,208,71,36,254,15,241,95,198,237,90,230,37,151,110,36,116,143,199,54,140,202,57,65,50,147,144,224,214,143,45,168,234,98,223,35,253,237,255,0,244,184,63,208,20,236,162,185,182,255,0,35,246,219,246,64,253,173,44,126,29,254,202,223,178,141,147,234,127,101,93,55,246,95,248,29,160,180,147,93,8,65,146,199,225,175,132,97,146,221,63,125,24,202,73,103,33,198,75,109,145,119,14,6,62,150,255,0,135,128,105,159,244,48,219,255,0,224,193,63,249,99,94,127,255,0,4,37,255,0,130,49,248,155,246,190,248,43,240,107,246,147,253,185,252,63,226,159,15,126,203,182,223,9,60,33,97,240,43,224,143,246,223,140,254,30,120,219,227,179,55,129,116,173,14,211,226,231,138,181,127,12,106,250,102,179,224,207,130,118,182,137,44,254,23,130,222,226,211,83,241,173,235,193,226,20,150,207,192,246,90,68,223,17,127,162,239,248,112,63,252,18,123,254,141,151,196,95,248,147,159,181,191,255,0,63,122,236,158,76,170,206,117,36,212,92,164,223,94,247,33,77,36,146,190,135,224,143,252,60,3,76,255,0,161,134,223,255,0,6,9,255,0,203,26,63,225,224,26,103,253,12,54,255,0,248,48,79,254,88,215,239,119,252,56,31,254,9,61,255,0,70,203,226,47,252,73,207,218,223,255,0,159,189,31,240,224,127,248,36,247,253,27,47,136,191,241,39,63,107,127,254,126,245,11,33,130,183,190,190,231,229,254,79,239,31,180,245,63,4,127,225,224,26,103,253,12,54,255,0,248,48,79,254,88,209,255,0,15,0,211,63,232,97,183,255,0,193,130,127,242,198,191,123,191,225,192,255,0,240,73,239,250,54,95,17,127,226,78,126,214,255,0,252,253,232,255,0,135,3,255,0,193,39,191,232,217,124,69,255,0,137,57,251,91,255,0,243,247,161,100,48,86,247,215,220,252,191,201,253,225,237,61,79,193,31,248,120,6,153,255,0,67,13,191,254,12,19,255,0,150,52,127,195,192,52,207,250,24,109,255,0,240,96,159,252,177,175,220,239,22,127,193,12,255,0,224,142,158,2,240,175,137,188,117,227,175,129,109,224,191,4,248,47,195,250,207,139,60,99,227,31,22,126,214,191,181,55,135,60,43,225,63,10,248,115,78,185,214,60,67,226,111,19,120,135,88,253,160,33,180,208,124,63,97,164,89,222,93,94,222,221,77,21,181,173,181,172,147,207,34,68,140,195,243,123,254,9,139,255,0,4,221,255,0,130,122,126,215,30,23,248,195,225,223,218,51,246,69,208,62,25,126,209,255,0,4,100,253,147,199,196,143,130,30,28,253,161,255,0,109,159,6,248,235,225,204,127,180,15,236,1,251,40,126,210,154,187,124,69,248,109,227,143,218,143,83,215,124,32,127,225,122,252,81,248,253,225,237,39,237,233,105,139,79,133,146,105,19,11,205,107,67,214,239,167,75,34,166,154,247,151,220,250,91,254,15,226,30,211,212,249,75,254,30,1,166,127,208,195,111,255,0,131,4,255,0,229,141,31,240,240,13,51,254,134,27,127,252,24,39,255,0,44,107,247,187,254,28,15,255,0,4,158,255,0,163,101,241,23,254,36,231,237,111,255,0,207,222,143,248,112,63,252,18,123,254,141,151,196,95,248,147,159,181,191,255,0,63,122,107,33,130,183,190,190,231,229,254,79,239,15,105,234,126,8,255,0,195,192,52,207,250,24,109,255,0,240,96,159,252,177,163,254,30,1,166,127,208,195,111,255,0,131,4,255,0,229,141,126,247,127,195,129,255,0,224,147,223,244,108,190,34,255,0,196,156,253,173,255,0,249,251,209,255,0,14,7,255,0,130,79,127,209,178,248,139,255,0,18,115,246,183,255,0,231,239,66,200,96,173,239,175,185,249,127,147,251,195,218,122,159,130,63,240,240,13,51,254,134,27,127,252,24,39,255,0,44,107,195,62,38,254,219,118,30,33,248,209,251,22,222,71,173,69,56,240,183,237,31,226,255,0,16,56,91,180,152,198,147,254,199,95,181,127,133,196,133,68,243,96,6,241,34,166,118,156,25,112,72,39,159,233,135,254,28,15,255,0,4,158,255,0,163,101,241,23,254,36,231,237,111,255,0,207,222,190,103,253,163,63,224,220,15,216,107,226,54,171,251,60,143,129,126,20,241,127,192,43,79,7,252,110,212,124,87,241,155,198,158,29,248,247,241,139,196,223,16,111,190,20,191,192,15,142,222,17,178,240,231,195,125,59,227,85,247,142,188,56,190,36,159,227,23,139,62,18,92,94,73,125,164,218,186,248,123,73,214,197,166,165,21,227,65,107,121,116,242,88,83,109,243,38,237,37,215,172,121,68,231,123,111,163,95,129,248,27,241,239,246,210,159,198,83,105,94,16,240,169,212,124,97,227,31,27,234,154,47,134,188,35,225,31,13,216,223,107,254,36,241,55,138,53,251,165,210,116,31,15,248,127,69,209,94,107,191,19,107,151,87,144,121,22,154,101,186,92,93,223,77,168,45,173,180,5,228,93,223,209,63,252,16,171,254,9,241,226,47,217,171,225,47,136,63,106,111,143,30,28,241,47,135,127,105,159,218,119,76,180,123,159,0,120,247,195,94,22,211,60,69,240,7,225,22,155,174,234,247,222,26,240,45,164,182,81,205,169,232,254,34,241,74,182,135,226,127,23,217,93,92,216,201,13,197,183,135,60,63,170,104,86,90,191,131,231,186,188,251,207,246,104,255,0,130,92,254,193,31,178,23,140,165,248,139,240,19,246,115,240,215,134,254,32,141,159,217,126,58,241,103,137,60,127,241,119,198,94,19,206,149,175,104,87,223,240,175,252,79,241,139,197,186,253,239,195,143,183,232,158,37,214,44,245,95,236,25,244,223,237,139,89,227,183,213,62,217,13,181,170,67,247,229,119,224,176,16,194,43,167,121,90,223,144,165,38,244,217,31,207,247,252,22,182,63,49,62,49,174,237,187,127,224,128,255,0,240,94,217,115,140,231,201,155,254,9,195,54,58,142,187,49,237,156,246,197,127,64,52,81,93,169,43,201,245,209,125,223,240,236,128,162,138,42,128,40,162,138,0,40,162,138,0,40,162,138,0,40,162,138,0,40,162,138,0,40,162,138,0,40,162,138,0,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 5505; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

