#ifndef GETVESUITE_HeatX_HeatX_E-HT-2_H
#define GETVESUITE_HeatX_HeatX_E-HT-2_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_HeatX_HeatX_E-HT-2( void )
{
    unsigned char osgData[ 4627 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,53,0,142,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,250,39,246,161,255,0,130,204,254,200,95,183,167,195,239,218,155,199,191,12,180,143,141,127,179,212,159,18,63,224,146,255,0,182,199,236,183,105,225,47,141,63,14,237,52,255,0,20,126,213,127,20,62,60,105,127,15,181,239,217,18,13,50,231,224,15,137,124,111,164,93,120,119,192,231,76,253,162,173,161,188,248,129,171,120,105,124,49,115,251,82,220,183,134,227,186,181,241,31,141,238,180,207,185,226,255,0,131,175,255,0,98,41,181,107,221,14,47,217,27,254,10,22,250,174,157,167,105,122,173,237,160,240,7,236,172,26,13,63,90,185,213,236,244,203,147,33,253,176,54,58,203,115,161,106,202,2,177,101,251,27,23,10,25,11,126,39,126,200,127,178,239,252,36,31,178,111,236,191,175,127,98,73,48,214,255,0,103,111,130,154,191,155,246,89,88,72,53,47,134,190,26,188,243,3,8,112,192,249,217,206,79,13,214,143,13,126,203,166,111,218,203,227,94,130,52,73,73,211,191,103,127,217,131,87,48,253,150,83,179,251,107,226,79,237,123,105,230,21,242,184,45,253,129,215,28,249,92,30,14,60,56,230,81,85,241,9,71,222,235,171,251,45,71,229,190,166,138,23,75,95,235,67,251,127,253,135,63,110,47,217,223,254,10,31,251,58,120,47,246,156,253,153,252,91,47,136,252,7,226,184,150,207,87,208,245,155,123,125,39,199,223,12,188,105,111,167,105,186,150,187,240,207,226,135,134,33,189,184,62,23,241,222,157,109,171,233,146,203,10,207,115,99,127,167,234,218,126,187,161,223,234,222,29,213,180,141,94,254,173,151,237,183,240,174,235,226,95,134,254,16,92,120,123,226,22,155,241,11,93,253,174,252,111,251,23,234,90,21,238,149,225,169,19,194,31,20,124,39,251,45,120,235,246,211,209,60,65,226,45,87,78,241,109,197,157,207,128,60,75,251,51,248,79,195,190,37,209,46,180,155,141,83,81,131,254,22,166,131,163,120,143,74,208,53,251,111,19,233,94,28,255,0,61,79,248,39,93,247,237,105,251,45,248,35,225,223,197,63,216,251,226,231,136,190,11,120,171,226,191,236,229,240,150,211,226,4,250,38,143,225,63,22,120,127,198,122,44,62,29,240,254,183,225,255,0,248,73,124,17,241,3,194,186,222,139,169,107,186,101,213,222,160,250,86,173,46,156,218,166,149,7,139,53,171,93,50,246,206,207,92,214,45,239,255,0,65,117,111,218,87,254,10,19,168,217,248,103,91,180,241,63,130,244,47,142,58,7,237,47,113,251,90,234,159,180,182,149,240,234,198,111,138,190,51,248,201,113,251,48,235,191,177,138,234,94,36,240,142,183,107,121,240,234,215,68,139,246,100,213,180,207,10,199,97,163,248,19,74,132,167,132,244,253,106,95,51,196,146,106,186,206,167,213,83,48,165,23,21,205,203,40,201,115,105,127,118,246,118,252,208,185,27,234,127,125,84,87,240,143,255,0,13,251,255,0,5,172,224,255,0,195,104,107,159,248,143,191,178,182,127,31,248,199,111,240,163,254,27,247,254,11,89,255,0,71,163,174,127,226,62,254,202,223,253,14,181,127,218,88,87,180,155,249,7,36,143,238,226,138,254,17,255,0,225,191,127,224,181,159,244,122,58,231,254,35,239,236,173,255,0,208,235,71,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,254,210,194,255,0,51,251,191,224,249,135,36,143,238,226,138,254,17,255,0,225,191,127,224,181,159,244,122,58,231,254,35,239,236,173,255,0,208,235,71,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,254,210,194,255,0,51,251,191,224,249,135,36,143,238,226,138,254,17,255,0,225,191,127,224,181,159,244,122,58,231,254,35,239,236,173,255,0,208,235,71,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,254,210,194,255,0,51,251,191,224,249,135,36,143,238,226,138,254,17,255,0,225,191,127,224,181,159,244,122,58,231,254,35,239,236,173,255,0,208,235,71,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,254,210,194,255,0,51,251,191,224,249,135,36,143,238,226,138,254,17,255,0,225,191,127,224,181,159,244,122,58,231,254,35,239,236,173,255,0,208,235,94,47,241,71,254,10,19,255,0,5,160,182,241,199,236,223,5,207,237,185,227,139,105,181,31,141,26,237,158,159,14,147,240,159,246,125,240,245,134,161,121,31,236,239,241,235,80,123,63,19,233,58,15,192,251,91,95,28,232,107,97,97,125,113,30,149,173,193,169,105,144,234,118,58,118,183,21,148,122,206,143,164,106,54,14,57,134,26,114,81,82,119,126,94,87,252,133,200,207,244,32,162,191,146,111,217,255,0,254,14,28,253,160,124,31,241,31,66,209,127,111,15,128,223,13,83,225,55,138,245,173,39,77,191,248,169,251,58,104,159,16,180,29,83,224,222,150,246,126,32,58,135,138,188,77,240,239,197,62,46,241,116,255,0,22,116,86,212,83,65,55,22,250,38,161,164,235,26,86,157,166,234,87,86,58,95,138,239,167,177,210,71,245,17,240,87,227,87,194,175,218,47,225,87,129,254,55,124,17,241,198,137,241,31,225,95,196,125,18,47,16,120,59,198,62,31,150,102,176,213,44,26,105,173,46,160,158,214,242,24,174,180,109,110,207,83,181,190,177,212,244,203,232,45,181,45,43,82,211,110,244,221,74,210,210,254,210,230,218,46,170,85,169,214,143,53,57,169,33,52,214,232,252,15,255,0,130,111,124,9,125,99,254,9,223,251,5,106,195,79,14,53,79,216,191,246,92,212,67,149,143,231,251,119,192,255,0,2,220,135,4,183,127,52,254,116,190,6,248,18,210,255,0,193,67,255,0,106,77,35,236,0,253,139,246,47,253,130,181,15,47,17,225,70,169,241,199,254,10,65,108,24,2,220,130,116,143,175,200,43,234,191,248,37,223,196,223,15,233,255,0,240,76,255,0,248,39,109,132,162,220,203,101,251,11,126,201,22,146,238,141,139,25,45,254,0,252,63,133,243,133,228,239,67,75,240,255,0,226,110,128,159,240,82,255,0,218,218,253,150,220,195,115,251,11,127,193,59,173,19,49,182,60,203,47,143,255,0,240,84,41,166,32,109,235,183,80,135,61,59,87,231,170,164,254,185,141,86,186,139,149,191,240,108,14,199,100,161,211,99,241,103,254,9,113,251,59,65,227,159,217,155,246,81,191,147,79,89,150,235,246,53,248,29,169,23,123,116,98,242,93,124,60,248,110,197,128,17,56,43,177,211,7,0,156,146,199,38,191,82,127,225,142,172,191,232,17,23,254,2,175,255,0,32,87,131,255,0,193,29,126,41,120,123,195,191,178,87,236,115,105,117,13,140,143,31,236,47,251,62,65,35,92,168,97,231,195,240,207,225,124,82,162,153,166,198,224,98,25,0,140,111,225,70,115,95,177,159,240,188,252,39,255,0,62,218,55,253,241,109,255,0,201,21,134,46,181,101,137,170,146,109,41,63,207,254,24,80,140,121,87,83,243,175,254,24,234,203,254,129,17,127,224,42,255,0,242,5,31,240,199,86,95,244,8,139,255,0,1,87,255,0,144,43,244,83,254,23,159,132,255,0,231,219,70,255,0,190,45,191,249,34,143,248,94,126,19,255,0,159,109,27,254,248,182,255,0,228,138,231,85,171,118,111,231,233,255,0,0,190,88,246,63,58,255,0,225,142,172,191,232,17,23,254,2,175,255,0,32,81,255,0,12,117,101,255,0,64,136,191,240,21,127,249,2,191,69,63,225,121,248,79,254,125,180,111,251,226,219,255,0,146,40,255,0,133,231,225,63,249,246,209,191,239,139,111,254,72,161,86,173,217,191,159,167,252,0,229,143,99,243,175,254,24,234,203,254,129,17,127,224,42,255,0,242,5,31,240,199,86,95,244,8,139,255,0,1,87,255,0,144,43,244,83,254,23,159,132,255,0,231,219,70,255,0,190,45,191,249,34,143,248,94,126,19,255,0,159,109,27,254,248,182,255,0,228,138,21,106,221,155,249,250,127,192,14,88,246,63,58,255,0,225,142,172,191,232,17,23,254,2,175,255,0,32,81,255,0,12,117,101,255,0,64,136,191,240,21,127,249,2,191,69,63,225,121,248,79,254,125,180,111,251,226,219,255,0,146,40,255,0,133,231,225,63,249,246,209,191,239,139,111,254,72,161,86,173,217,191,159,167,252,0,229,143,99,243,175,254,24,234,203,254,129,17,127,224,42,255,0,242,5,31,240,199,86,95,244,8,139,255,0,1,87,255,0,144,43,244,83,254,23,159,132,255,0,231,219,70,255,0,190,45,191,249,34,143,248,94,126,19,255,0,159,109,27,254,248,182,255,0,228,138,21,106,221,155,249,250,127,192,14,88,246,63,58,255,0,225,142,172,191,232,17,23,254,2,175,255,0,32,87,199,159,180,231,236,175,101,162,252,99,255,0,130,124,219,157,41,2,235,191,181,231,142,52,185,17,109,227,83,42,90,254,192,127,183,23,136,138,13,214,138,9,206,130,24,6,56,204,96,156,99,112,253,215,255,0,133,231,225,63,249,246,209,191,239,139,111,254,72,175,135,127,107,63,139,126,24,214,62,60,127,193,50,130,69,166,197,30,157,251,110,124,70,212,110,90,5,137,91,200,95,248,38,143,252,20,58,200,7,49,207,194,121,215,176,245,249,114,1,108,140,169,232,194,214,170,235,70,233,217,70,127,250,67,37,198,54,219,183,230,143,201,239,218,159,246,88,210,44,52,125,66,25,180,244,251,130,2,179,159,45,34,84,48,22,101,95,56,60,69,101,93,209,200,190,113,95,182,25,35,1,149,150,189,35,254,13,227,253,160,126,35,248,63,246,129,248,243,251,7,107,90,238,181,226,191,132,201,240,215,92,253,162,254,21,216,234,90,181,155,105,127,7,53,77,7,226,15,132,60,47,241,23,195,62,22,211,135,135,197,196,186,47,139,47,254,46,248,127,90,184,183,23,246,218,126,149,172,120,79,84,190,181,211,174,47,188,87,171,95,47,213,191,181,191,141,124,60,108,117,102,129,96,143,207,18,197,31,217,36,128,155,135,217,60,74,0,147,202,47,135,159,106,145,184,48,129,202,159,151,21,252,200,93,126,195,159,22,127,224,168,127,180,206,177,251,59,124,0,241,15,195,191,8,120,219,194,159,15,252,69,241,163,81,213,62,48,234,222,37,240,255,0,133,167,240,191,134,188,81,225,31,7,95,216,89,94,248,43,194,62,32,187,151,95,109,79,226,143,135,222,24,164,177,138,220,193,103,120,207,117,28,145,195,21,207,191,146,86,170,231,21,102,247,86,191,77,12,106,164,175,166,199,235,135,236,27,251,74,233,250,15,236,55,251,24,232,111,119,42,182,139,251,40,126,206,186,83,168,104,176,173,167,124,32,240,125,163,40,204,227,128,208,144,50,7,165,30,15,253,165,244,248,127,110,79,218,39,92,251,92,187,117,31,217,71,246,50,210,131,110,143,37,180,111,139,223,183,149,227,169,63,104,25,1,117,228,247,249,206,64,226,191,156,47,217,43,246,158,248,149,226,175,217,183,79,147,225,103,195,31,137,31,19,124,37,251,49,252,16,248,124,255,0,27,252,87,240,247,194,94,41,241,151,134,190,15,120,119,65,240,29,211,63,136,126,40,235,126,27,208,238,173,126,30,232,99,77,240,71,139,46,126,213,171,77,105,110,45,252,49,168,77,230,152,236,174,94,63,31,210,127,224,164,94,12,179,248,209,227,255,0,27,55,136,180,113,107,226,15,134,31,8,188,45,20,199,94,180,242,164,159,193,222,43,248,223,171,92,196,178,236,196,142,145,248,230,209,153,71,42,39,82,120,113,90,44,182,126,223,17,62,75,169,95,241,148,95,252,17,41,171,36,229,177,251,31,251,22,254,213,182,159,13,255,0,103,47,217,138,210,227,85,134,209,236,191,101,239,131,58,17,15,114,177,178,181,151,195,223,4,38,221,198,116,218,12,22,214,255,0,40,111,249,101,202,2,51,95,95,127,195,192,52,207,250,24,109,255,0,240,96,159,252,177,174,15,254,8,13,255,0,4,104,213,255,0,106,159,130,254,7,253,165,255,0,111,47,134,247,214,127,179,38,173,240,19,193,190,17,253,156,190,15,106,158,35,248,167,240,227,226,47,196,125,64,89,120,50,97,251,71,222,106,159,14,252,111,160,223,248,91,225,144,209,52,45,79,76,240,189,149,249,188,111,24,218,248,178,127,19,69,107,167,104,22,30,20,213,188,89,253,21,127,195,129,255,0,224,147,223,244,108,190,34,255,0,196,156,253,173,255,0,249,251,215,92,178,101,82,82,156,154,78,77,189,111,222,253,4,167,100,151,99,240,71,254,30,1,166,127,208,195,111,255,0,131,4,255,0,229,141,31,240,240,13,51,254,134,27,127,252,24,39,255,0,44,107,247,187,254,28,15,255,0,4,158,255,0,163,101,241,23,254,36,231,237,111,255,0,207,222,143,248,112,63,252,18,123,254,141,151,196,95,248,147,159,181,191,255,0,63,122,159,236,40,127,50,252,124,191,200,61,167,169,248,35,255,0,15,0,211,63,232,97,183,255,0,193,130,127,242,198,143,248,120,6,153,255,0,67,13,191,254,12,19,255,0,150,53,251,221,255,0,14,7,255,0,130,79,127,209,178,248,139,255,0,18,115,246,183,255,0,231,239,71,252,56,31,254,9,61,255,0,70,203,226,47,252,73,207,218,223,255,0,159,189,31,216,80,254,101,248,249,127,144,123,79,83,240,71,254,30,1,166,127,208,195,111,255,0,131,4,255,0,229,141,31,240,240,13,51,254,134,27,127,252,24,39,255,0,44,107,247,187,254,28,15,255,0,4,158,255,0,163,101,241,23,254,36,231,237,111,255,0,207,222,143,248,112,63,252,18,123,254,141,151,196,95,248,147,159,181,191,255,0,63,122,63,176,161,252,203,241,242,255,0,32,246,158,167,224,143,252,60,3,76,255,0,161,134,223,255,0,6,9,255,0,203,26,63,225,224,26,103,253,12,54,255,0,248,48,79,254,88,215,239,119,252,56,31,254,9,61,255,0,70,203,226,47,252,73,207,218,223,255,0,159,189,31,240,224,127,248,36,247,253,27,47,136,191,241,39,63,107,127,254,126,244,127,97,67,249,151,227,229,254,65,237,61,79,193,31,248,120,6,153,255,0,67,13,191,254,12,19,255,0,150,52,127,195,192,52,207,250,24,109,255,0,240,96,159,252,177,175,222,239,248,112,63,252,18,123,254,141,151,196,95,248,147,159,181,191,255,0,63,122,63,225,192,255,0,240,73,239,250,54,111,17,127,226,78,126,214,255,0,252,253,232,254,194,135,243,47,199,203,252,131,218,122,159,130,63,240,240,29,51,254,134,43,127,252,24,167,255,0,44,107,194,254,38,254,219,86,30,33,248,207,251,22,222,71,173,69,113,255,0,8,175,237,31,226,255,0,16,56,75,180,152,198,179,254,199,95,181,127,134,60,194,191,104,155,3,119,137,17,115,179,254,90,99,35,53,250,61,251,126,255,0,193,55,255,0,224,158,191,179,26,252,109,63,7,255,0,100,77,7,198,39,224,151,252,19,31,246,231,253,180,188,104,60,71,251,67,126,219,126,33,255,0,132,39,226,127,193,153,62,18,175,236,161,161,248,252,120,95,246,162,211,255,0,225,27,248,127,227,164,143,246,163,48,219,93,253,147,80,241,63,252,40,93,108,248,111,85,181,30,22,241,21,125,91,251,70,127,193,184,63,176,223,196,93,87,246,122,31,2,252,41,226,255,0,128,86,158,15,248,221,169,120,179,227,55,141,60,59,241,239,227,23,137,190,32,223,124,41,147,224,7,199,111,8,217,248,115,225,190,157,241,170,251,199,94,28,95,18,79,241,139,197,159,9,46,47,37,190,210,109,93,124,61,164,235,98,211,82,134,241,160,181,188,116,178,122,112,171,59,52,221,53,217,253,168,217,89,191,196,29,75,171,127,90,88,252,12,248,245,251,104,220,120,202,93,43,194,62,21,58,143,140,60,99,227,109,83,69,240,207,132,124,35,225,187,27,237,127,196,158,38,241,70,191,118,186,78,133,225,255,0,15,104,186,43,205,119,226,125,118,234,242,31,34,211,77,182,75,139,203,233,245,5,181,182,128,201,34,239,254,137,255,0,224,133,95,240,79,127,16,254,205,95,9,124,65,251,83,124,120,240,231,137,188,59,251,76,254,211,154,93,163,92,120,3,199,190,26,240,182,153,226,47,128,63,8,180,221,123,87,191,240,215,129,45,38,178,138,109,79,70,241,23,138,81,180,63,19,248,190,202,234,230,198,72,110,45,188,57,225,253,83,65,178,213,252,31,61,213,231,222,127,179,71,252,18,227,246,8,253,144,188,101,39,196,95,128,159,179,159,134,188,55,241,4,108,254,202,241,215,139,60,73,227,239,139,190,50,240,151,252,74,181,237,10,247,254,21,255,0,137,254,49,120,183,95,189,248,113,246,253,19,196,218,197,158,171,253,131,62,155,253,177,105,60,118,250,167,219,33,182,181,72,126,252,175,67,5,151,211,194,43,173,103,111,151,200,137,73,201,159,207,231,252,22,186,33,50,252,97,82,118,236,255,0,130,3,255,0,193,123,166,7,25,207,147,39,252,19,138,77,164,103,161,0,140,246,206,121,233,95,208,29,20,87,114,140,84,165,36,173,41,90,254,118,216,144,162,138,42,128,40,162,138,0,40,162,138,0,40,162,138,0,40,162,138,0,40,162,138,0,40,162,138,0,40,162,138,0,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 4627; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

