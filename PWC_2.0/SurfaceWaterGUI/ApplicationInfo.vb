

Public Class ApplicationInfo

    Public Const MaximumApplications As Integer = 50  'cvalue used for GUI
    Private Const TotalMaxApplications As Integer = 5000  'Maximum applications for the entire simulations, only needed for vvwm transfers & multicrop routines
    Public Const NumberofApplicationTypes As Integer = 7

    Public Shared ReadOnly day As TextBox() = {Form1.day1, Form1.day2, Form1.day3, Form1.day4, Form1.day5,
                    Form1.day6, Form1.day7, Form1.day8, Form1.day9, Form1.day10,
                    Form1.day11, Form1.day12, Form1.day13, Form1.day14, Form1.day15,
                    Form1.day16, Form1.day17, Form1.day18, Form1.day19, Form1.day20,
                    Form1.day21, Form1.day22, Form1.day23, Form1.day24, Form1.day25,
                      Form1.day26, Form1.day27, Form1.day28, Form1.day29, Form1.day30,
                      Form1.day31, Form1.day32, Form1.day33, Form1.day34, Form1.day35,
                      Form1.day36, Form1.day37, Form1.day38, Form1.day39, Form1.day40,
                      Form1.day41, Form1.day42, Form1.day43, Form1.day44, Form1.day45,
                      Form1.day46, Form1.day47, Form1.day48, Form1.day49, Form1.day50}

    Public Shared ReadOnly mon As TextBox() = {Form1.Mon1, Form1.Mon2, Form1.Mon3, Form1.Mon4, Form1.Mon5,
                       Form1.Mon6, Form1.Mon7, Form1.Mon8, Form1.Mon9, Form1.Mon10,
                       Form1.Mon11, Form1.Mon12, Form1.Mon13, Form1.Mon14, Form1.Mon15,
                       Form1.Mon16, Form1.Mon17, Form1.Mon18, Form1.Mon19, Form1.Mon20,
                       Form1.Mon21, Form1.Mon22, Form1.Mon23, Form1.Mon24, Form1.Mon25,
                      Form1.mon26, Form1.mon27, Form1.mon28, Form1.mon29, Form1.mon30,
                      Form1.mon31, Form1.mon32, Form1.mon33, Form1.mon34, Form1.mon35,
                      Form1.mon36, Form1.mon37, Form1.mon38, Form1.mon39, Form1.mon40,
                      Form1.mon41, Form1.mon42, Form1.mon43, Form1.mon44, Form1.mon45,
                      Form1.mon46, Form1.mon47, Form1.mon48, Form1.mon49, Form1.mon50}


    Public Shared ReadOnly year As TextBox() = {Form1.year1, Form1.year2, Form1.year3, Form1.year4, Form1.year5,
                   Form1.year6, Form1.year7, Form1.year8, Form1.year9, Form1.year10,
                   Form1.year11, Form1.year12, Form1.year13, Form1.year14, Form1.year15,
                   Form1.year16, Form1.year17, Form1.year18, Form1.year19, Form1.year20,
                   Form1.year21, Form1.year22, Form1.year23, Form1.year24, Form1.year25,
                  Form1.year26, Form1.year27, Form1.year28, Form1.year29, Form1.year30,
                  Form1.year31, Form1.year32, Form1.year33, Form1.year34, Form1.year35,
                  Form1.year36, Form1.year37, Form1.year38, Form1.year39, Form1.year40,
                  Form1.year41, Form1.year42, Form1.year43, Form1.year44, Form1.year45,
                  Form1.year46, Form1.year47, Form1.year48, Form1.year49, Form1.year50}


    Public Shared ReadOnly rate As TextBox() = {Form1.rate1, Form1.rate2, Form1.rate3, Form1.rate4, Form1.rate5,
                      Form1.rate6, Form1.rate7, Form1.rate8, Form1.rate9, Form1.rate10,
                      Form1.rate11, Form1.rate12, Form1.rate13, Form1.rate14, Form1.rate15,
                      Form1.rate16, Form1.rate17, Form1.rate18, Form1.rate19, Form1.rate20,
                      Form1.rate21, Form1.rate22, Form1.rate23, Form1.rate24, Form1.rate25,
                      Form1.rate26, Form1.rate27, Form1.rate28, Form1.rate29, Form1.rate30,
                      Form1.rate31, Form1.rate32, Form1.rate33, Form1.rate34, Form1.rate35,
                      Form1.rate36, Form1.rate37, Form1.rate38, Form1.rate39, Form1.rate40,
                      Form1.rate41, Form1.rate42, Form1.rate43, Form1.rate44, Form1.rate45,
                      Form1.rate46, Form1.rate47, Form1.rate48, Form1.rate49, Form1.rate50}



    Public Shared ReadOnly spray As TextBox() = {Form1.spray1, Form1.spray2, Form1.spray3, Form1.spray4, Form1.spray5,
                      Form1.spray6, Form1.spray7, Form1.spray8, Form1.spray9, Form1.spray10,
                      Form1.spray11, Form1.spray12, Form1.spray13, Form1.spray14, Form1.spray15,
                      Form1.spray16, Form1.spray17, Form1.spray18, Form1.spray19, Form1.spray20,
                      Form1.spray21, Form1.spray22, Form1.spray23, Form1.spray24, Form1.spray25,
                      Form1.spray26, Form1.spray27, Form1.spray28, Form1.spray29, Form1.spray30,
                      Form1.spray31, Form1.spray32, Form1.spray33, Form1.spray34, Form1.spray35,
                      Form1.spray36, Form1.spray37, Form1.spray38, Form1.spray39, Form1.spray40,
                      Form1.spray41, Form1.spray42, Form1.spray43, Form1.spray44, Form1.spray45,
                      Form1.spray46, Form1.spray47, Form1.spray48, Form1.spray49, Form1.spray50}

    Public Shared ReadOnly sprayp As TextBox() = {Form1.sprayp1, Form1.sprayp2, Form1.sprayp3, Form1.sprayp4, Form1.sprayp5,
                  Form1.sprayp6, Form1.sprayp7, Form1.sprayp8, Form1.sprayp9, Form1.sprayp10,
                  Form1.sprayp11, Form1.sprayp12, Form1.sprayp13, Form1.sprayp14, Form1.sprayp15,
                  Form1.sprayp16, Form1.sprayp17, Form1.sprayp18, Form1.sprayp19, Form1.sprayp20,
                  Form1.sprayp21, Form1.sprayp22, Form1.sprayp23, Form1.sprayp24, Form1.sprayp25,
                  Form1.sprayp26, Form1.sprayp27, Form1.sprayp28, Form1.sprayp29, Form1.sprayp30,
                  Form1.sprayp31, Form1.sprayp32, Form1.sprayp33, Form1.sprayp34, Form1.sprayp35,
                  Form1.sprayp36, Form1.sprayp37, Form1.sprayp38, Form1.sprayp39, Form1.sprayp40,
                  Form1.sprayp41, Form1.sprayp42, Form1.sprayp43, Form1.sprayp44, Form1.sprayp45,
                  Form1.sprayp46, Form1.sprayp47, Form1.sprayp48, Form1.sprayp49, Form1.sprayp50}

    Public Shared ReadOnly sprayr As TextBox() = {Form1.sprayr1, Form1.sprayr2, Form1.sprayr3, Form1.sprayr4, Form1.sprayr5,
                  Form1.sprayr6, Form1.sprayr7, Form1.sprayr8, Form1.sprayr9, Form1.sprayr10,
                  Form1.sprayr11, Form1.sprayr12, Form1.sprayr13, Form1.sprayr14, Form1.sprayr15,
                  Form1.sprayr16, Form1.sprayr17, Form1.sprayr18, Form1.sprayr19, Form1.sprayr20,
                  Form1.sprayr21, Form1.sprayr22, Form1.sprayr23, Form1.sprayr24, Form1.sprayr25,
                  Form1.sprayr26, Form1.sprayr27, Form1.sprayr28, Form1.sprayr29, Form1.sprayr30,
                  Form1.sprayr31, Form1.sprayr32, Form1.sprayr33, Form1.sprayr34, Form1.sprayr35,
                  Form1.sprayr36, Form1.sprayr37, Form1.sprayr38, Form1.sprayr39, Form1.sprayr40,
                  Form1.sprayr41, Form1.sprayr42, Form1.sprayr43, Form1.sprayr44, Form1.sprayr45,
                  Form1.sprayr46, Form1.sprayr47, Form1.sprayr48, Form1.sprayr49, Form1.sprayr50}


    Public Shared ReadOnly Eff As TextBox() = {Form1.Eff1, Form1.Eff2, Form1.Eff3, Form1.Eff4, Form1.Eff5,
                      Form1.Eff6, Form1.Eff7, Form1.Eff8, Form1.Eff9, Form1.Eff10,
                      Form1.Eff11, Form1.Eff12, Form1.Eff13, Form1.Eff14, Form1.Eff15,
                      Form1.Eff16, Form1.Eff17, Form1.Eff18, Form1.Eff19, Form1.Eff20,
                      Form1.Eff21, Form1.Eff22, Form1.Eff23, Form1.Eff24, Form1.Eff25,
                      Form1.Eff26, Form1.Eff27, Form1.Eff28, Form1.Eff29, Form1.Eff30,
                      Form1.Eff31, Form1.Eff32, Form1.Eff33, Form1.Eff34, Form1.Eff35,
                      Form1.Eff36, Form1.Eff37, Form1.Eff38, Form1.Eff39, Form1.Eff40,
                      Form1.Eff41, Form1.Eff42, Form1.Eff43, Form1.Eff44, Form1.Eff45,
                      Form1.Eff46, Form1.Eff47, Form1.Eff48, Form1.Eff49, Form1.Eff50}

    Public Shared ReadOnly Effp As TextBox() = {Form1.Effp1, Form1.Effp2, Form1.Effp3, Form1.Effp4, Form1.Effp5,
              Form1.Effp6, Form1.Effp7, Form1.Effp8, Form1.Effp9, Form1.Effp10,
              Form1.Effp11, Form1.Effp12, Form1.Effp13, Form1.Effp14, Form1.Effp15,
              Form1.Effp16, Form1.Effp17, Form1.Effp18, Form1.Effp19, Form1.Effp20,
              Form1.Effp21, Form1.Effp22, Form1.Effp23, Form1.Effp24, Form1.Effp25,
              Form1.Effp26, Form1.Effp27, Form1.Effp28, Form1.Effp29, Form1.Effp30,
              Form1.Effp31, Form1.Effp32, Form1.Effp33, Form1.Effp34, Form1.Effp35,
              Form1.Effp36, Form1.Effp37, Form1.Effp38, Form1.Effp39, Form1.Effp40,
              Form1.Effp41, Form1.Effp42, Form1.Effp43, Form1.Effp44, Form1.Effp45,
              Form1.Effp46, Form1.Effp47, Form1.Effp48, Form1.Effp49, Form1.Effp50}

    Public Shared ReadOnly Effr As TextBox() = {Form1.Effr1, Form1.Effr2, Form1.Effr3, Form1.Effr4, Form1.Effr5,
              Form1.Effr6, Form1.Effr7, Form1.Effr8, Form1.Effr9, Form1.Effr10,
              Form1.Effr11, Form1.Effr12, Form1.Effr13, Form1.Effr14, Form1.Effr15,
              Form1.Effr16, Form1.Effr17, Form1.Effr18, Form1.Effr19, Form1.Effr20,
              Form1.Effr21, Form1.Effr22, Form1.Effr23, Form1.Effr24, Form1.Effr25,
              Form1.Effr26, Form1.Effr27, Form1.Effr28, Form1.Effr29, Form1.Effr30,
              Form1.Effr31, Form1.Effr32, Form1.Effr33, Form1.Effr34, Form1.Effr35,
              Form1.Effr36, Form1.Effr37, Form1.Effr38, Form1.Effr39, Form1.Effr40,
              Form1.Effr41, Form1.Effr42, Form1.Effr43, Form1.Effr44, Form1.Effr45,
              Form1.Effr46, Form1.Effr47, Form1.Effr48, Form1.Effr49, Form1.Effr50}



    Public Shared ReadOnly RelApp As TextBox() = {Form1.RelDay1, Form1.RelDay2, Form1.RelDay3, Form1.RelDay4, Form1.RelDay5,
                     Form1.RelDay6, Form1.RelDay7, Form1.RelDay8, Form1.RelDay9, Form1.RelDay10,
                     Form1.RelDay11, Form1.RelDay12, Form1.RelDay13, Form1.RelDay14, Form1.RelDay15,
                     Form1.RelDay16, Form1.RelDay17, Form1.RelDay18, Form1.RelDay19, Form1.RelDay20,
                     Form1.RelDay21, Form1.RelDay22, Form1.RelDay23, Form1.RelDay24, Form1.RelDay25,
                      Form1.RelDay26, Form1.RelDay27, Form1.RelDay28, Form1.RelDay29, Form1.RelDay30,
                      Form1.RelDay31, Form1.RelDay32, Form1.RelDay33, Form1.RelDay34, Form1.RelDay35,
                      Form1.RelDay36, Form1.RelDay37, Form1.RelDay38, Form1.RelDay39, Form1.RelDay40,
                      Form1.RelDay41, Form1.RelDay42, Form1.RelDay43, Form1.RelDay44, Form1.RelDay45,
                      Form1.RelDay46, Form1.RelDay47, Form1.RelDay48, Form1.RelDay49, Form1.RelDay50}



    Public Shared ReadOnly methodbutton As RadioButton() =
        {Form1.RB11, Form1.RB12, Form1.RB13, Form1.RB14, Form1.RB15, Form1.RB16, Form1.RB17,
         Form1.RB21, Form1.RB22, Form1.RB23, Form1.RB24, Form1.RB25, Form1.RB26, Form1.RB27,
         Form1.RB31, Form1.RB32, Form1.RB33, Form1.RB34, Form1.RB35, Form1.RB36, Form1.RB37,
         Form1.RB41, Form1.RB42, Form1.RB43, Form1.RB44, Form1.RB45, Form1.RB46, Form1.RB47,
         Form1.RB51, Form1.RB52, Form1.RB53, Form1.RB54, Form1.RB55, Form1.RB56, Form1.RB57,
         Form1.RB61, Form1.RB62, Form1.RB63, Form1.RB64, Form1.RB65, Form1.RB66, Form1.RB67,
         Form1.RB71, Form1.RB72, Form1.RB73, Form1.RB74, Form1.RB75, Form1.RB76, Form1.RB77,
         Form1.RB81, Form1.RB82, Form1.RB83, Form1.RB84, Form1.RB85, Form1.RB86, Form1.RB87,
         Form1.RB91, Form1.RB92, Form1.RB93, Form1.RB94, Form1.RB95, Form1.RB96, Form1.RB97,
         Form1.RB101, Form1.RB102, Form1.RB103, Form1.RB104, Form1.RB105, Form1.RB106, Form1.RB107,
         Form1.RB111, Form1.RB112, Form1.RB113, Form1.RB114, Form1.RB115, Form1.RB116, Form1.RB117,
         Form1.RB121, Form1.RB122, Form1.RB123, Form1.RB124, Form1.RB125, Form1.RB126, Form1.RB127,
         Form1.RB131, Form1.RB132, Form1.RB133, Form1.RB134, Form1.RB135, Form1.RB136, Form1.RB137,
         Form1.RB141, Form1.RB142, Form1.RB143, Form1.RB144, Form1.RB145, Form1.RB146, Form1.RB147,
         Form1.RB151, Form1.RB152, Form1.RB153, Form1.RB154, Form1.RB155, Form1.RB156, Form1.RB157,
         Form1.RB161, Form1.RB162, Form1.RB163, Form1.RB164, Form1.RB165, Form1.RB166, Form1.RB167,
         Form1.RB171, Form1.RB172, Form1.RB173, Form1.RB174, Form1.RB175, Form1.RB176, Form1.RB177,
         Form1.RB181, Form1.RB182, Form1.RB183, Form1.RB184, Form1.RB185, Form1.RB186, Form1.RB187,
         Form1.RB191, Form1.RB192, Form1.RB193, Form1.RB194, Form1.RB195, Form1.RB196, Form1.RB197,
         Form1.RB201, Form1.RB202, Form1.RB203, Form1.RB204, Form1.RB205, Form1.RB206, Form1.RB207,
         Form1.RB211, Form1.RB212, Form1.RB213, Form1.RB214, Form1.RB215, Form1.RB216, Form1.RB217,
         Form1.RB221, Form1.RB222, Form1.RB223, Form1.RB224, Form1.RB225, Form1.RB226, Form1.RB227,
         Form1.RB231, Form1.RB232, Form1.RB233, Form1.RB234, Form1.RB235, Form1.RB236, Form1.RB237,
         Form1.RB241, Form1.RB242, Form1.RB243, Form1.RB244, Form1.RB245, Form1.RB246, Form1.RB247,
         Form1.RB251, Form1.RB252, Form1.RB253, Form1.RB254, Form1.RB255, Form1.RB256, Form1.RB257,
         Form1.RB261, Form1.RB262, Form1.RB263, Form1.RB264, Form1.RB265, Form1.RB266, Form1.RB267,
         Form1.RB271, Form1.RB272, Form1.RB273, Form1.RB274, Form1.RB275, Form1.RB276, Form1.RB277,
         Form1.RB281, Form1.RB282, Form1.RB283, Form1.RB284, Form1.RB285, Form1.RB286, Form1.RB287,
         Form1.RB291, Form1.RB292, Form1.RB293, Form1.RB294, Form1.RB295, Form1.RB296, Form1.RB297,
         Form1.RB301, Form1.RB302, Form1.RB303, Form1.RB304, Form1.RB305, Form1.RB306, Form1.RB307,
         Form1.RB311, Form1.RB312, Form1.RB313, Form1.RB314, Form1.RB315, Form1.RB316, Form1.RB317,
         Form1.RB321, Form1.RB322, Form1.RB323, Form1.RB324, Form1.RB325, Form1.RB326, Form1.RB327,
         Form1.RB331, Form1.RB332, Form1.RB333, Form1.RB334, Form1.RB335, Form1.RB336, Form1.RB337,
         Form1.RB341, Form1.RB342, Form1.RB343, Form1.RB344, Form1.RB345, Form1.RB346, Form1.RB347,
         Form1.RB351, Form1.RB352, Form1.RB353, Form1.RB354, Form1.RB355, Form1.RB356, Form1.RB357,
         Form1.RB361, Form1.RB362, Form1.RB363, Form1.RB364, Form1.RB365, Form1.RB366, Form1.RB367,
         Form1.RB371, Form1.RB372, Form1.RB373, Form1.RB374, Form1.RB375, Form1.RB376, Form1.RB377,
         Form1.RB381, Form1.RB382, Form1.RB383, Form1.RB384, Form1.RB385, Form1.RB386, Form1.RB387,
         Form1.RB391, Form1.RB392, Form1.RB393, Form1.RB394, Form1.RB395, Form1.RB396, Form1.RB397,
         Form1.RB401, Form1.RB402, Form1.RB403, Form1.RB404, Form1.RB405, Form1.RB406, Form1.RB407,
         Form1.RB411, Form1.RB412, Form1.RB413, Form1.RB414, Form1.RB415, Form1.RB416, Form1.RB417,
         Form1.RB421, Form1.RB422, Form1.RB423, Form1.RB424, Form1.RB425, Form1.RB426, Form1.RB427,
         Form1.RB431, Form1.RB432, Form1.RB433, Form1.RB434, Form1.RB435, Form1.RB436, Form1.RB437,
         Form1.RB441, Form1.RB442, Form1.RB443, Form1.RB444, Form1.RB445, Form1.RB446, Form1.RB447,
         Form1.RB451, Form1.RB452, Form1.RB453, Form1.RB454, Form1.RB455, Form1.RB456, Form1.RB457,
         Form1.RB461, Form1.RB462, Form1.RB463, Form1.RB464, Form1.RB465, Form1.RB466, Form1.RB467,
         Form1.RB471, Form1.RB472, Form1.RB473, Form1.RB474, Form1.RB475, Form1.RB476, Form1.RB477,
         Form1.RB481, Form1.RB482, Form1.RB483, Form1.RB484, Form1.RB485, Form1.RB486, Form1.RB487,
         Form1.RB491, Form1.RB492, Form1.RB493, Form1.RB494, Form1.RB495, Form1.RB496, Form1.RB497,
         Form1.RB501, Form1.RB502, Form1.RB503, Form1.RB504, Form1.RB505, Form1.RB506, Form1.RB507}

    Public Shared ReadOnly MethodPanels As Panel() = {Form1.PanelM1, Form1.PanelM2, Form1.PanelM3, Form1.PanelM4, Form1.PanelM5,
                 Form1.PanelM6, Form1.PanelM7, Form1.PanelM8, Form1.PanelM9, Form1.PanelM10,
                 Form1.PanelM11, Form1.PanelM12, Form1.PanelM13, Form1.PanelM14, Form1.PanelM15,
                 Form1.PanelM16, Form1.PanelM17, Form1.PanelM18, Form1.PanelM19, Form1.PanelM20,
                 Form1.PanelM21, Form1.PanelM22, Form1.PanelM23, Form1.PanelM24, Form1.PanelM25,
                 Form1.PanelM26, Form1.PanelM27, Form1.PanelM28, Form1.PanelM29, Form1.PanelM30,
                 Form1.PanelM31, Form1.PanelM32, Form1.PanelM33, Form1.PanelM34, Form1.PanelM35,
                 Form1.PanelM36, Form1.PanelM37, Form1.PanelM38, Form1.PanelM39, Form1.PanelM40,
                 Form1.PanelM41, Form1.PanelM42, Form1.PanelM43, Form1.PanelM44, Form1.PanelM45,
                 Form1.PanelM46, Form1.PanelM47, Form1.PanelM48, Form1.PanelM49, Form1.PanelM50}



    Public Shared ReadOnly DepthIncorp As TextBox() = {Form1.depth1, Form1.depth2, Form1.depth3, Form1.depth4, Form1.depth5,
                 Form1.depth6, Form1.depth7, Form1.depth8, Form1.depth9, Form1.depth10,
                 Form1.depth11, Form1.depth12, Form1.depth13, Form1.depth14, Form1.depth15,
                 Form1.depth16, Form1.depth17, Form1.depth18, Form1.depth19, Form1.depth20,
                 Form1.depth21, Form1.depth22, Form1.depth23, Form1.depth24, Form1.depth25,
                 Form1.depth26, Form1.depth27, Form1.depth28, Form1.depth29, Form1.depth30,
                 Form1.depth31, Form1.depth32, Form1.depth33, Form1.depth34, Form1.depth35,
                 Form1.depth36, Form1.depth37, Form1.depth38, Form1.depth39, Form1.depth40,
                 Form1.depth41, Form1.depth42, Form1.depth43, Form1.depth44, Form1.depth45,
                 Form1.depth46, Form1.depth47, Form1.depth48, Form1.depth49, Form1.depth50}


    Public Shared ReadOnly tBand As TextBox() = {Form1.tBand1, Form1.tBand2, Form1.tBand3, Form1.tBand4, Form1.tBand5,
                     Form1.tBand6, Form1.tBand7, Form1.tBand8, Form1.tBand9, Form1.tBand10,
                     Form1.tBand11, Form1.tBand12, Form1.tBand13, Form1.tBand14, Form1.tBand15,
                     Form1.tBand16, Form1.tBand17, Form1.tBand18, Form1.tBand19, Form1.tBand20,
                     Form1.tBand21, Form1.tBand22, Form1.tBand23, Form1.tBand24, Form1.tBand25,
                      Form1.tBand26, Form1.tBand27, Form1.tBand28, Form1.tBand29, Form1.tBand30,
                      Form1.tBand31, Form1.tBand32, Form1.tBand33, Form1.tBand34, Form1.tBand35,
                      Form1.tBand36, Form1.tBand37, Form1.tBand38, Form1.tBand39, Form1.tBand40,
                      Form1.tBand41, Form1.tBand42, Form1.tBand43, Form1.tBand44, Form1.tBand45,
                      Form1.tBand46, Form1.tBand47, Form1.tBand48, Form1.tBand49, Form1.tBand50}





    Private Shared xxx As Date
    Public Shared Property startdate As Date
        Get
            Return xxx
        End Get

        Set(value As Date)
            xxx = value
        End Set
    End Property


    Private Shared sss(TotalMaxApplications) As Single
    Public Shared Property sprayTotalArray() As Single()
        Get
            Return sss
        End Get

        Set(value As Single())
            sss = value
        End Set

    End Property


    Private Shared ttt(TotalMaxApplications) As Single
    Public Shared Property rateTotalArray() As Single()
        Get
            Return ttt
        End Get

        Set(value As Single())
            ttt = value
        End Set

    End Property



    Private Shared aaa(TotalMaxApplications) As Date
    Public Shared Property applicationDates() As Date()
        Get
            Return aaa
        End Get

        Set(value As Date())
            aaa = value
        End Set

    End Property


    Private Shared bbb As Integer
    Public Shared Property totalApplications As Integer
        Get
            Return bbb
        End Get

        Set(value As Integer)
            bbb = value
        End Set
    End Property



    Private Shared ggg As Single
    Public Shared Property TotalMassApplied() As Single

        Get
            Return ggg
        End Get

        Set(value As Single)
            ggg = value
        End Set

    End Property


    Private Shared pad(TotalMaxApplications) As Integer
    Public Shared Property PestAppyDay() As Integer()
        Get
            Return pad
        End Get

        Set(value As Integer())
            pad = value
        End Set
    End Property


    Private Shared pam(TotalMaxApplications) As Integer
    Public Shared Property PestAppyMon() As Integer()
        Get
            Return pam
        End Get

        Set(value As Integer())
            pam = value
        End Set

    End Property


    Private Shared hhh As String
    Public Shared Property EmergenceMaturityHarvest As String

        Get
            Return hhh
        End Get

        Set(value As String)
            hhh = value
        End Set

    End Property






    Public Shared Function AppInfoString() As String
        Dim msg As String

        msg = String.Format("{0}{1}", Form1.appNumber.Text, vbNewLine)

        For i As Integer = 0 To MaximumApplications - 1
            msg = msg & day(i).Text & ","
        Next
        msg = msg & vbNewLine

        For i As Integer = 0 To MaximumApplications - 1
            msg = msg & mon(i).Text & ","
        Next

        msg = msg & vbNewLine

        For i As Integer = 0 To MaximumApplications - 1
            msg = msg & rate(i).Text & ","
        Next


        msg = msg & vbNewLine & Form1.SpecifyYears.Checked

        msg = msg & vbNewLine
        For i As Integer = 0 To MaximumApplications - 1
            msg = msg & year(i).Text & ","
        Next



        Dim submsg As String
        submsg = ""
        For i As Integer = 0 To MaximumApplications * NumberofApplicationTypes - 1 Step NumberofApplicationTypes

            If methodbutton(i).Checked Then
                submsg = submsg & "1,"
            ElseIf methodbutton(i + 1).Checked Then
                submsg = submsg & "2,"
            ElseIf methodbutton(i + 2).Checked Then
                submsg = submsg & "3,"
            ElseIf methodbutton(i + 3).Checked Then
                submsg = submsg & "4,"
            ElseIf methodbutton(i + 4).Checked Then
                submsg = submsg & "5,"
            ElseIf methodbutton(i + 5).Checked Then
                submsg = submsg & "6,"
            ElseIf methodbutton(i + 6).Checked Then
                submsg = submsg & "7,"


            Else
                submsg = submsg & ","
            End If

        Next

        msg = msg & vbNewLine & submsg  'ConvertAppMethod2integer()



        '************Custom Water Body values
        msg = msg & vbNewLine
        For i As Integer = 0 To MaximumApplications - 1
            msg = msg & Eff(i).Text & ","
        Next

        msg = msg & vbNewLine
        For i As Integer = 0 To MaximumApplications - 1
            msg = msg & spray(i).Text & ","
        Next
        msg = msg & vbNewLine



        '***********Pond Water Body values
        For i As Integer = 0 To MaximumApplications - 1
            '    msg = msg & Eff(i).Text & ","
            msg = msg & Effp(i).Text & ","
        Next

        msg = msg & vbNewLine

        For i As Integer = 0 To MaximumApplications - 1
            ' msg = msg & spray(i).Text & ","
            msg = msg & sprayp(i).Text & ","
        Next
        msg = msg & vbNewLine

        '*************Reservoir Water Body values
        For i As Integer = 0 To MaximumApplications - 1
            msg = msg & Effr(i).Text & ","
        Next

        msg = msg & vbNewLine
        For i As Integer = 0 To MaximumApplications - 1
            msg = msg & sprayr(i).Text & ","
        Next

        msg = msg & vbNewLine


        For i As Integer = 0 To MaximumApplications - 1
            msg = msg & DepthIncorp(i).Text & ","
        Next
        msg = msg & vbNewLine

        For i As Integer = 0 To MaximumApplications - 1
            msg = msg & RelApp(i).Text & ","
        Next

        Dim relAppType As Integer
        If Form1.RelToEmerge.Checked Then
            relAppType = 1
        ElseIf Form1.RelToMat.Checked Then
            relAppType = 2
        Else
            relAppType = 3
        End If


        msg = msg & String.Format("{0}{1}, {2}", vbNewLine, Form1.AppsAbsolute.Checked, relAppType)

        msg = msg & vbNewLine
        For i As Integer = 0 To MaximumApplications - 1
            msg = msg & tBand(i).Text & ","
        Next



        msg = msg & vbNewLine & "," 'blank line for expansion
        msg = msg & vbNewLine & "," 'blank line for expansion
        msg = msg & vbNewLine & "," 'blank line for expansion

        Return msg
    End Function




    Public Shared Sub GetApplicationMethodProperties(ByVal j As Integer, ByRef cam As Integer, ByRef depi As Single, ByRef tband_local As Single)
        'For each pesticide apply event, this finds which radio button (which represents the pest apply method) is checked and delivers the default


        If methodbutton(j * NumberofApplicationTypes).Checked Then
            cam = 1
            depi = 4.0
            tband_local = 0.0
        ElseIf methodbutton(j * NumberofApplicationTypes + 1).Checked Then
            cam = 2
            depi = 4.0
            tband_local = 0.0
        ElseIf methodbutton(j * NumberofApplicationTypes + 2).Checked Then
            cam = 4
            depi = DepthIncorp(j).Text
            tband_local = 0.0
        ElseIf methodbutton(j * NumberofApplicationTypes + 3).Checked Then
            cam = 8
            depi = DepthIncorp(j).Text
            tband_local = 0.0
        ElseIf methodbutton(j * NumberofApplicationTypes + 4).Checked Then
            cam = 7 'T BAND
            depi = DepthIncorp(j).Text
            tband_local = tBand(j).Text

        ElseIf methodbutton(j * NumberofApplicationTypes + 5).Checked Then
            cam = 5  'linearly increasing
            depi = DepthIncorp(j).Text
            tband_local = 0.0

        ElseIf methodbutton(j * NumberofApplicationTypes + 6).Checked Then
            cam = 6  'linearly decreasing with depth of incorp specified
            depi = DepthIncorp(j).Text
            tband_local = 0.0
        Else
        End If




    End Sub












End Class
