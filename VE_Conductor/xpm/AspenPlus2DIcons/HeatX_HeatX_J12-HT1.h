#ifndef GETVESUITE_HeatX_HeatX_J12-HT1_H
#define GETVESUITE_HeatX_HeatX_J12-HT1_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_HeatX_HeatX_J12-HT1( void )
{
    unsigned char osgData[ 5646 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,60,0,147,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,250,43,246,160,255,0,130,205,126,200,127,183,167,195,255,0,218,155,199,191,12,116,143,141,127,179,204,159,18,63,224,146,255,0,182,191,236,183,105,225,63,141,95,14,237,52,255,0,20,126,213,127,20,62,60,233,127,15,181,239,217,18,13,50,231,224,23,137,60,113,163,221,120,119,192,231,75,253,162,109,161,189,241,254,173,225,165,240,189,207,237,77,114,222,27,142,234,215,196,126,54,186,211,62,230,178,255,0,131,175,255,0,98,45,70,231,87,179,179,253,145,127,224,161,147,220,232,90,132,122,86,173,18,248,3,246,85,83,103,168,77,164,233,154,228,118,238,207,251,96,1,35,29,43,89,211,38,220,133,148,11,160,165,183,171,170,254,39,126,200,95,178,233,241,7,236,155,251,47,107,223,216,114,202,53,191,217,223,224,166,175,230,139,89,91,205,26,151,195,95,12,222,23,14,34,59,131,9,243,156,243,187,52,191,5,63,101,211,169,124,74,253,175,108,191,177,36,127,248,71,255,0,104,143,12,232,229,126,203,33,49,121,223,178,119,236,191,175,121,101,124,191,151,39,91,15,131,130,4,160,227,158,124,69,153,70,53,49,13,69,94,50,73,234,250,53,21,167,77,223,204,211,146,233,107,107,159,219,247,236,59,251,113,126,206,255,0,240,80,239,217,211,193,127,180,231,236,207,226,217,124,71,224,63,21,196,182,90,190,135,172,219,65,164,248,251,225,151,141,109,244,237,55,82,215,126,25,252,80,240,188,87,151,7,194,222,59,211,173,181,125,50,89,97,89,238,108,175,236,53,109,63,93,208,239,245,111,14,234,218,70,175,127,245,213,127,155,55,252,18,143,246,158,253,179,191,96,95,134,190,14,214,255,0,102,43,223,9,235,190,6,248,213,240,67,225,135,137,60,107,240,135,226,158,147,226,111,21,124,42,187,241,204,126,9,240,28,118,31,22,180,125,27,194,254,36,210,111,252,53,241,45,60,57,111,30,137,123,121,99,168,193,109,173,105,137,103,111,175,218,106,175,225,255,0,11,73,160,254,190,127,195,241,255,0,224,170,159,244,71,63,99,111,252,54,63,30,191,249,254,215,127,215,240,215,180,170,114,201,110,187,19,202,236,157,175,115,251,29,162,191,142,47,248,126,63,252,21,83,254,136,231,236,109,255,0,134,203,227,215,255,0,63,202,63,225,248,255,0,240,85,79,250,35,159,177,183,254,27,47,143,95,252,255,0,40,250,254,23,254,126,175,199,250,234,28,178,236,127,99,180,87,241,197,255,0,15,199,255,0,130,170,127,209,28,253,141,191,240,217,124,122,255,0,231,249,71,252,63,31,254,10,169,255,0,68,115,246,54,255,0,195,101,241,235,255,0,159,229,31,95,194,255,0,207,213,248,255,0,93,67,150,93,143,235,119,226,151,140,188,69,240,255,0,192,154,239,139,188,39,240,159,226,7,199,31,16,105,63,217,159,217,255,0,11,126,22,234,95,11,52,143,29,248,163,237,250,206,159,166,93,127,97,106,31,26,254,37,248,63,195,54,255,0,98,178,189,185,212,110,191,180,252,71,167,111,179,210,46,18,203,237,122,131,90,88,221,124,251,251,40,126,219,31,10,255,0,109,11,143,138,122,151,193,13,3,226,13,255,0,195,31,134,239,240,73,52,111,140,254,35,210,188,57,225,255,0,2,124,90,111,142,223,179,183,194,207,218,147,195,159,240,174,180,11,143,21,191,140,44,6,157,240,139,227,119,194,139,173,95,254,18,191,11,120,99,203,188,241,156,118,58,119,246,148,246,26,176,211,255,0,154,79,248,126,63,252,21,83,254,136,231,236,109,248,252,50,248,244,63,247,190,87,205,223,179,87,237,249,251,95,254,196,154,15,138,254,27,254,202,63,5,126,20,88,124,35,215,95,224,123,233,122,71,237,10,124,123,241,131,226,54,148,126,7,254,200,31,179,95,236,117,163,253,191,199,191,13,239,126,30,105,154,192,190,240,103,236,201,225,77,86,231,203,240,189,150,205,79,196,26,128,139,22,191,102,134,21,245,252,53,215,239,85,189,30,250,91,243,14,89,118,63,186,234,43,248,226,255,0,135,227,255,0,193,85,63,232,142,126,198,223,248,108,190,61,127,243,252,163,254,31,143,255,0,5,84,255,0,162,57,251,27,127,225,178,248,245,255,0,207,242,159,215,240,191,243,245,126,63,215,80,229,151,99,251,29,162,191,142,47,248,126,63,252,21,83,254,136,231,236,109,255,0,134,203,227,215,255,0,63,202,63,225,248,255,0,240,85,79,250,35,159,177,183,254,27,47,143,95,252,255,0,40,250,254,23,254,126,175,199,250,234,28,178,236,127,99,180,87,241,197,255,0,15,199,255,0,130,170,127,209,28,253,141,191,240,217,124,122,255,0,231,249,95,56,126,214,127,240,85,207,248,41,175,237,47,240,23,226,7,236,221,227,47,9,254,205,95,15,254,31,252,126,208,181,95,132,159,18,181,239,135,63,12,62,35,199,227,155,175,134,254,50,210,53,29,59,199,94,21,240,246,163,227,255,0,138,250,214,153,162,54,185,225,163,168,232,215,183,205,165,93,95,218,233,154,237,236,186,44,218,86,182,186,118,183,166,159,95,194,255,0,207,213,253,127,195,135,36,187,31,172,255,0,19,63,224,232,207,248,39,199,130,190,40,252,73,248,113,240,247,225,151,237,93,251,74,104,159,13,60,89,47,130,174,254,50,252,1,240,143,192,189,99,224,223,140,53,187,61,35,73,213,53,89,190,30,120,147,226,39,237,15,225,171,255,0,23,120,114,9,117,100,183,135,90,131,74,26,62,175,246,99,168,248,126,255,0,87,208,238,116,253,86,243,207,52,143,248,58,255,0,246,34,215,116,157,51,92,210,127,100,95,248,40,101,230,149,172,233,246,90,174,153,120,158,1,253,149,99,75,189,63,81,182,142,242,202,229,35,159,246,192,87,69,146,218,88,216,43,170,176,15,134,80,65,3,249,242,253,135,127,101,213,212,236,63,104,187,40,180,48,201,225,223,143,218,86,136,145,193,3,200,150,193,191,102,127,217,199,91,251,62,228,67,243,143,237,156,176,227,44,231,229,25,197,122,223,236,135,251,46,127,194,65,251,38,254,203,250,247,246,28,178,141,111,246,119,248,41,171,121,162,214,86,243,70,165,240,219,195,55,165,247,8,126,96,222,126,115,206,119,103,61,235,142,174,104,169,243,218,41,242,202,43,239,141,251,148,161,181,222,255,0,240,15,213,255,0,21,255,0,193,229,95,240,77,47,4,107,247,254,23,241,87,236,229,255,0,5,2,210,53,237,51,236,191,111,211,165,248,101,251,54,220,73,111,246,219,43,109,70,215,116,214,127,181,124,145,190,251,43,187,121,6,215,108,9,64,56,96,64,43,252,253,191,224,171,254,16,111,1,254,223,191,30,252,41,246,119,181,254,202,255,0,133,91,251,130,141,25,79,183,124,22,248,115,169,125,194,163,110,126,217,187,160,251,212,87,169,78,126,210,157,57,223,227,73,253,233,63,215,241,33,171,54,187,31,233,71,255,0,4,222,248,18,250,191,252,19,191,246,10,213,133,128,113,170,126,197,255,0,178,230,162,28,172,103,127,219,126,7,248,26,228,55,39,156,249,191,173,31,178,231,192,153,47,190,56,255,0,193,72,45,133,128,111,236,127,219,67,192,186,123,46,216,200,79,51,254,9,225,251,5,234,161,72,207,28,106,96,255,0,192,205,125,89,255,0,4,188,248,155,160,88,127,193,51,191,224,157,182,19,139,99,61,151,236,45,251,36,90,76,76,108,91,205,182,248,3,240,254,25,9,59,121,59,208,230,151,246,73,248,155,225,251,111,143,223,240,84,41,164,22,219,111,255,0,110,143,135,215,112,230,50,64,137,63,224,153,255,0,240,78,235,19,180,109,225,124,235,25,120,245,205,126,122,167,83,219,99,252,166,191,244,244,127,67,178,203,220,86,210,223,161,248,137,255,0,4,186,253,155,237,252,115,251,55,254,202,183,38,197,89,175,255,0,99,47,129,250,193,144,197,49,44,183,30,0,248,115,33,39,201,108,177,121,111,38,35,12,0,17,130,83,113,227,245,55,254,24,182,15,249,241,143,254,252,95,127,241,127,231,21,230,31,240,69,223,30,248,79,74,253,147,63,99,228,189,180,181,185,186,183,253,133,191,103,219,41,76,165,206,214,139,225,167,194,193,32,198,230,57,18,68,228,158,23,231,232,43,246,171,254,22,183,130,191,232,19,167,123,124,178,116,255,0,191,124,214,56,188,69,101,137,170,162,172,148,159,230,40,40,180,154,87,63,41,63,225,139,96,255,0,159,24,255,0,239,197,247,255,0,23,71,252,49,108,31,243,227,31,253,248,190,255,0,226,235,245,111,254,22,183,130,127,232,21,167,127,223,50,127,241,186,63,225,107,120,39,254,129,90,119,253,243,39,255,0,27,174,111,172,215,234,191,173,10,228,137,249,73,255,0,12,91,7,252,248,199,255,0,126,47,191,248,186,63,225,139,96,255,0,159,24,255,0,239,197,247,255,0,23,95,171,127,240,181,188,19,255,0,64,173,59,254,249,147,255,0,141,209,255,0,11,91,193,63,244,10,211,191,239,153,63,248,221,31,89,175,219,250,211,250,254,152,114,68,252,164,255,0,134,45,131,254,124,163,255,0,191,23,223,252,93,31,240,197,176,127,207,140,127,247,226,251,185,207,247,235,245,111,254,22,183,130,127,232,21,167,127,223,50,127,241,186,63,225,107,120,39,254,129,90,119,253,243,39,255,0,27,161,98,107,255,0,95,32,228,137,249,73,255,0,12,91,7,252,248,199,255,0,126,47,191,248,186,63,225,139,96,255,0,159,24,255,0,239,197,247,255,0,23,95,171,127,240,181,188,19,255,0,64,173,59,254,249,147,255,0,141,209,255,0,11,91,193,63,244,10,211,191,239,153,63,248,221,31,89,175,219,79,248,111,235,250,97,201,19,242,147,254,24,182,15,249,241,143,254,252,95,127,241,116,127,195,22,193,255,0,62,49,255,0,223,139,239,254,46,191,86,255,0,225,107,120,39,254,129,90,119,253,243,39,255,0,27,163,254,22,183,130,127,232,21,167,127,223,50,127,241,186,62,179,95,183,245,167,245,253,48,228,137,249,73,255,0,12,91,7,252,248,199,255,0,126,47,191,248,186,240,47,218,39,246,69,183,208,188,31,161,95,54,158,140,191,240,152,88,192,248,134,228,16,179,104,158,33,5,255,0,123,184,28,1,156,0,73,198,56,235,95,187,31,240,181,188,19,255,0,64,173,59,254,249,147,255,0,141,215,205,255,0,181,15,196,175,4,95,124,63,210,33,109,50,197,54,120,199,79,148,50,101,91,141,23,196,9,140,186,14,62,127,92,250,14,181,84,241,21,156,225,126,173,122,116,254,190,241,56,198,207,200,252,145,255,0,130,109,252,12,109,67,83,253,188,173,214,192,48,210,63,108,237,11,78,32,42,5,71,147,246,20,253,136,245,89,21,67,54,66,151,212,203,15,103,231,6,189,147,254,9,191,240,41,245,127,248,39,135,236,21,171,11,0,227,84,253,139,255,0,101,189,68,62,216,254,127,182,252,14,240,45,200,110,79,57,243,115,248,215,167,127,193,51,254,35,248,127,78,241,55,252,20,92,73,246,118,75,207,219,163,195,119,112,150,86,127,221,143,248,39,159,236,11,102,219,73,103,32,121,214,114,241,184,242,59,116,175,95,255,0,130,94,252,77,240,254,159,255,0,4,207,255,0,130,119,88,76,182,198,107,47,216,91,246,72,180,148,152,201,111,54,223,224,15,195,248,100,36,237,228,239,67,154,238,198,212,169,203,86,203,254,94,83,255,0,211,65,20,189,221,58,63,204,255,0,52,79,248,56,83,195,135,194,191,240,88,15,218,239,64,104,252,147,97,255,0,10,11,247,127,47,203,246,175,217,131,224,173,232,232,221,197,192,63,141,21,217,127,193,202,154,181,174,187,255,0,5,173,253,180,117,91,77,130,222,235,254,25,207,203,9,144,191,184,253,147,126,4,91,54,6,222,62,120,91,241,162,190,215,8,219,194,225,155,209,186,112,255,0,210,81,199,45,223,169,253,121,254,193,191,180,182,159,161,126,195,127,177,142,134,247,115,43,232,223,178,135,236,237,165,58,6,139,10,218,119,194,15,7,217,178,140,220,3,128,208,250,15,165,31,179,183,237,45,167,233,223,23,191,111,27,198,186,149,70,187,251,87,120,63,85,70,45,18,239,16,254,195,127,177,150,135,188,254,255,0,174,253,25,186,103,133,3,61,135,242,197,240,67,254,10,69,224,207,3,124,23,248,67,224,155,175,17,233,16,92,248,63,225,127,128,124,45,115,4,186,245,172,50,67,113,225,255,0,10,105,58,76,209,73,11,140,197,34,201,102,192,169,251,164,99,181,47,128,63,224,164,62,12,240,255,0,138,254,55,106,210,248,143,71,142,63,29,124,80,210,124,83,104,239,174,218,160,154,222,207,224,191,194,31,5,60,145,182,220,76,159,106,240,117,196,123,151,3,116,44,156,20,57,241,158,91,83,218,98,31,39,241,36,154,249,77,63,208,215,158,252,190,246,199,237,183,236,53,251,84,89,252,54,253,156,63,101,248,39,213,126,204,182,31,178,191,193,109,4,129,44,143,39,238,62,31,120,37,81,60,184,100,93,168,178,216,93,14,113,141,160,16,195,6,190,212,255,0,134,255,0,210,63,232,61,38,120,207,252,126,30,222,166,239,154,249,251,254,8,41,255,0,4,104,241,95,237,103,240,99,225,15,237,47,251,114,120,87,95,240,199,236,191,7,194,127,11,105,159,2,126,8,220,106,158,40,240,87,141,254,61,163,248,47,76,210,45,62,48,248,179,83,208,53,27,13,91,192,255,0,5,45,227,251,93,207,133,45,237,174,109,181,95,25,221,92,65,226,21,155,79,240,69,174,152,126,33,125,9,225,15,217,159,254,9,253,227,31,248,41,167,143,191,98,77,59,246,23,240,108,159,8,124,45,251,94,124,34,248,61,225,79,142,22,95,180,71,237,129,168,159,137,31,12,60,123,255,0,4,249,253,186,190,50,124,65,180,240,219,193,241,245,116,217,252,127,224,159,219,95,246,9,248,173,224,93,111,88,181,158,255,0,78,139,251,47,95,240,77,255,0,135,244,159,22,248,83,81,213,174,58,39,147,123,73,74,164,164,162,228,246,221,234,214,246,254,144,149,68,146,74,250,127,192,25,255,0,13,255,0,164,127,208,122,79,252,156,255,0,228,186,63,225,191,244,143,250,15,73,255,0,147,159,252,151,95,184,127,240,225,223,248,37,71,253,27,5,255,0,254,36,23,237,61,255,0,207,162,143,248,112,239,252,18,163,254,141,130,255,0,255,0,18,11,246,158,255,0,231,209,75,251,6,26,123,235,238,244,242,242,15,105,234,126,30,127,195,127,233,31,244,30,147,255,0,39,63,249,46,143,248,111,253,35,254,131,210,127,228,231,255,0,37,215,238,31,252,56,119,254,9,81,255,0,70,193,127,255,0,137,5,251,79,127,243,232,163,254,28,59,255,0,4,168,255,0,163,96,191,255,0,196,130,253,167,191,249,244,81,253,131,13,61,245,247,122,121,7,180,245,63,15,63,225,191,244,143,250,15,73,255,0,147,159,252,151,71,252,55,254,145,255,0,65,233,63,242,115,255,0,146,235,247,15,254,28,59,255,0,4,168,255,0,163,96,191,255,0,196,130,253,167,191,249,244,81,255,0,14,29,255,0,130,84,127,209,176,95,255,0,226,65,126,211,223,252,250,40,254,193,134,158,250,251,189,60,131,218,122,159,135,159,240,223,250,71,253,7,164,255,0,201,207,254,75,163,254,27,255,0,72,255,0,160,244,159,249,57,255,0,201,117,251,135,255,0,14,29,255,0,130,84,127,209,176,95,255,0,226,65,126,211,223,252,250,40,255,0,135,14,255,0,193,42,63,232,216,47,255,0,241,32,191,105,239,254,125,20,127,96,195,79,125,125,222,158,65,237,61,79,195,207,248,111,253,35,254,131,210,127,228,231,255,0,37,209,255,0,13,255,0,164,127,208,122,79,252,156,255,0,228,186,253,195,255,0,135,14,255,0,193,42,63,232,216,47,255,0,241,32,191,105,239,254,125,20,127,195,135,127,224,149,31,244,108,23,255,0,248,144,95,180,247,255,0,62,138,63,176,97,167,190,190,239,79,32,246,158,167,225,231,252,55,254,145,255,0,65,233,63,242,115,255,0,146,235,207,190,35,126,219,154,103,138,116,173,43,75,139,90,50,145,174,219,221,58,72,247,17,169,138,11,13,72,73,243,73,59,13,197,36,32,99,7,39,239,1,154,233,62,46,124,54,255,0,130,64,248,11,254,10,215,251,51,126,192,30,31,253,159,126,22,107,255,0,6,62,61,107,199,225,206,169,241,218,223,246,149,253,163,181,95,12,120,79,246,128,248,117,225,47,218,250,215,227,199,236,181,172,124,79,178,253,174,97,210,116,31,218,118,195,226,173,159,252,19,215,79,211,188,17,38,141,121,173,218,219,252,114,213,45,53,152,32,212,124,77,224,128,63,80,191,107,223,248,55,147,246,78,241,231,193,77,114,211,246,48,209,36,253,156,63,104,205,6,238,15,19,252,61,241,95,136,126,37,124,97,241,247,195,191,23,223,233,150,122,132,18,252,49,248,169,164,248,227,197,94,34,151,71,240,30,179,21,233,142,77,111,66,179,26,230,131,168,89,233,218,196,86,250,254,155,103,169,248,75,196,34,200,163,29,83,78,75,109,31,144,123,79,35,242,155,246,27,253,163,172,244,93,115,246,208,150,123,201,60,221,111,246,171,208,53,114,82,72,156,51,39,236,103,251,32,232,210,13,198,101,206,37,210,36,81,140,240,128,100,227,53,232,223,176,111,237,45,167,232,63,176,223,236,101,161,61,220,171,38,141,251,40,254,206,218,83,160,49,97,91,79,248,67,224,251,70,80,13,192,32,3,23,28,15,160,175,230,30,203,246,154,241,207,236,93,241,83,246,142,248,23,251,73,248,87,86,248,51,241,171,66,248,203,105,55,142,190,30,120,234,250,203,77,215,116,109,78,223,224,231,194,63,14,173,204,50,195,115,113,107,226,15,15,94,219,248,126,43,221,39,89,211,46,111,52,109,107,75,212,45,53,93,26,250,251,74,188,180,187,155,187,253,158,63,105,223,137,122,79,236,103,224,239,138,218,79,195,47,137,26,191,192,95,130,94,15,248,111,240,175,226,23,198,253,47,194,94,42,190,248,65,224,95,24,233,126,26,240,31,135,226,240,175,140,62,36,217,232,82,104,190,23,241,27,222,248,163,194,17,37,133,237,244,55,70,79,21,233,177,136,203,95,90,9,176,196,101,213,37,26,151,131,119,156,30,221,160,151,252,2,149,77,173,43,37,127,208,252,211,255,0,130,223,248,190,31,26,255,0,193,80,191,105,223,19,68,90,84,212,255,0,225,75,109,145,246,238,111,177,126,207,63,9,180,242,78,214,35,239,90,30,231,129,69,124,121,251,102,252,82,179,248,213,251,74,124,72,248,153,166,93,69,123,99,226,95,248,67,252,139,155,107,132,187,130,95,236,111,1,120,91,195,242,121,119,17,0,178,98,109,42,69,56,232,84,175,106,43,232,104,211,229,163,70,45,107,24,197,125,201,127,193,254,182,197,217,183,169,254,234,212,81,69,116,8,43,224,15,135,63,242,148,223,219,35,254,204,3,254,9,167,255,0,173,21,255,0,5,98,175,191,235,248,131,255,0,130,253,255,0,193,106,255,0,106,127,248,35,231,252,21,53,63,225,154,60,3,251,63,248,227,254,26,35,246,0,253,150,255,0,225,53,255,0,133,233,225,95,136,190,38,254,203,255,0,133,73,251,69,126,221,255,0,240,142,127,194,45,255,0,8,7,197,95,12,125,139,207,255,0,133,155,175,125,187,237,127,110,243,62,201,103,228,125,155,203,155,237,0,31,219,229,21,254,96,159,241,26,183,252,21,55,254,136,31,236,1,255,0,134,179,246,138,255,0,232,170,163,254,35,86,255,0,130,166,255,0,209,3,253,128,63,240,214,126,209,95,253,21,84,1,254,159,116,87,249,130,127,196,106,223,240,84,223,250,32,127,176,7,254,26,207,218,43,255,0,162,170,143,248,141,91,254,10,155,255,0,68,15,246,0,255,0,195,89,251,69,127,244,85,80,7,250,125,209,95,230,9,255,0,17,171,127,193,83,127,232,129,254,192,31,248,107,63,104,175,254,138,170,63,226,53,111,248,42,111,253,16,63,216,3,255,0,13,103,237,21,255,0,209,85,64,31,233,247,69,127,152,39,252,70,173,255,0,5,77,255,0,162,7,251,0,127,225,172,253,162,191,250,42,168,255,0,136,213,191,224,169,191,244,64,255,0,96,15,252,53,159,180,87,255,0,69,85,0,127,167,221,21,254,96,159,241,26,183,252,21,55,254,136,31,236,1,255,0,134,179,246,138,255,0,232,170,163,254,35,86,255,0,130,166,255,0,209,3,253,128,63,240,214,126,209,95,253,21,84,1,253,254,124,70,255,0,148,166,254,198,255,0,246,96,31,240,82,207,253,104,175,248,36,237,125,255,0,95,196,31,252,16,19,254,11,87,251,83,255,0,193,96,255,0,224,169,175,255,0,13,47,224,31,217,255,0,192,255,0,240,206,255,0,176,7,237,73,255,0,8,87,252,40,191,10,252,69,240,207,246,167,252,45,191,218,43,246,16,255,0,132,143,254,18,159,248,79,254,42,248,159,237,190,71,252,43,45,7,236,63,100,251,15,151,246,187,207,63,237,62,100,63,103,254,223,40,0,175,201,191,218,115,246,46,248,253,241,135,246,168,241,143,196,175,7,92,124,31,183,248,81,227,125,11,254,9,33,119,121,169,248,155,199,62,52,211,62,33,104,222,42,255,0,130,115,255,0,193,76,188,109,251,93,248,235,69,182,240,62,151,240,170,255,0,77,213,252,63,226,47,131,127,19,252,67,6,147,169,183,137,45,46,109,252,77,225,43,45,46,243,73,93,43,87,159,196,26,55,235,37,20,165,21,37,102,174,147,79,230,154,107,241,64,20,81,69,48,63,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 5646; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

