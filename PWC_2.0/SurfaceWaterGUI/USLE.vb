Public Class USLE

    Public Const MaxHydroErosionFactors As Integer = 52


    Public Shared ReadOnly day As TextBox() = {Form1.dayFactor1, Form1.dayFactor2, Form1.dayFactor3, Form1.dayFactor4, Form1.dayFactor5, Form1.dayFactor6, Form1.dayFactor7, Form1.dayFactor8,
                               Form1.dayFactor9, Form1.dayFactor10, Form1.dayFactor11, Form1.dayFactor12, Form1.dayFactor13, Form1.dayFactor14, Form1.dayFactor15, Form1.dayFactor16,
                               Form1.dayFactor17, Form1.dayFactor18, Form1.dayFactor19, Form1.dayFactor20, Form1.dayFactor21, Form1.dayFactor22, Form1.dayFactor23, Form1.dayFactor24,
                               Form1.dayFactor25, Form1.dayFactor26, Form1.dayFactor27, Form1.dayFactor28, Form1.dayFactor29, Form1.dayFactor30, Form1.dayFactor31, Form1.dayFactor32,
                               Form1.dayFactor33, Form1.dayFactor34, Form1.dayFactor35, Form1.dayFactor36, Form1.dayFactor37, Form1.dayFactor38, Form1.dayFactor39, Form1.dayFactor40,
                               Form1.dayFactor41, Form1.dayFactor42, Form1.dayFactor43, Form1.dayFactor44, Form1.dayFactor45, Form1.dayFactor46, Form1.dayFactor47, Form1.dayFactor48,
                               Form1.dayFactor49, Form1.dayFactor50, Form1.dayFactor51, Form1.dayFactor52}


    Public Shared ReadOnly mon As TextBox() = {Form1.monFactor1, Form1.monFactor2, Form1.monFactor3, Form1.monFactor4, Form1.monFactor5, Form1.monFactor6, Form1.monFactor7, Form1.monFactor8,
                            Form1.monFactor9, Form1.monFactor10, Form1.monFactor11, Form1.monFactor12, Form1.monFactor13, Form1.monFactor14, Form1.monFactor15, Form1.monFactor16,
                            Form1.monFactor17, Form1.monFactor18, Form1.monFactor19, Form1.monFactor20, Form1.monFactor21, Form1.monFactor22, Form1.monFactor23, Form1.monFactor24,
                            Form1.monFactor25, Form1.monFactor26, Form1.monFactor27, Form1.monFactor28, Form1.monFactor29, Form1.monFactor30, Form1.monFactor31, Form1.monFactor32,
                            Form1.monFactor33, Form1.monFactor34, Form1.monFactor35, Form1.monFactor36, Form1.monFactor37, Form1.monFactor38, Form1.monFactor39, Form1.monFactor40,
                            Form1.monFactor41, Form1.monFactor42, Form1.monFactor43, Form1.monFactor44, Form1.monFactor45, Form1.monFactor46, Form1.monFactor47, Form1.monFactor48,
                            Form1.monFactor49, Form1.monFactor50, Form1.monFactor51, Form1.monFactor52}

    Public Shared ReadOnly year As TextBox() = {Form1.YearFactor1, Form1.YearFactor2, Form1.YearFactor3, Form1.YearFactor4, Form1.YearFactor5, Form1.YearFactor6, Form1.YearFactor7, Form1.YearFactor8,
                           Form1.YearFactor9, Form1.YearFactor10, Form1.YearFactor11, Form1.YearFactor12, Form1.YearFactor13, Form1.YearFactor14, Form1.YearFactor15, Form1.YearFactor16,
                           Form1.YearFactor17, Form1.YearFactor18, Form1.YearFactor19, Form1.YearFactor20, Form1.YearFactor21, Form1.YearFactor22, Form1.YearFactor23, Form1.YearFactor24,
                           Form1.YearFactor25, Form1.YearFactor26, Form1.YearFactor27, Form1.YearFactor28, Form1.YearFactor29, Form1.YearFactor30, Form1.YearFactor31, Form1.YearFactor32,
                           Form1.YearFactor33, Form1.YearFactor34, Form1.YearFactor35, Form1.YearFactor36, Form1.YearFactor37, Form1.YearFactor38, Form1.YearFactor39, Form1.YearFactor40,
                           Form1.YearFactor41, Form1.YearFactor42, Form1.YearFactor43, Form1.YearFactor44, Form1.YearFactor45, Form1.YearFactor46, Form1.YearFactor47, Form1.YearFactor48,
                           Form1.YearFactor49, Form1.YearFactor50, Form1.YearFactor51, Form1.YearFactor52}


    Public Shared ReadOnly C As TextBox() = {Form1.cFactor1, Form1.cFactor2, Form1.cFactor3, Form1.cFactor4, Form1.cFactor5, Form1.cFactor6, Form1.cFactor7, Form1.cFactor8,
                           Form1.cFactor9, Form1.cFactor10, Form1.cFactor11, Form1.cFactor12, Form1.cFactor13, Form1.cFactor14, Form1.cFactor15, Form1.cFactor16,
                           Form1.cFactor17, Form1.cFactor18, Form1.cFactor19, Form1.cFactor20, Form1.cFactor21, Form1.cFactor22, Form1.cFactor23, Form1.cFactor24,
                           Form1.cFactor25, Form1.cFactor26, Form1.cFactor27, Form1.cFactor28, Form1.cFactor29, Form1.cFactor30, Form1.cFactor31, Form1.cFactor32,
                           Form1.cFactor33, Form1.cFactor34, Form1.cFactor35, Form1.cFactor36, Form1.cFactor37, Form1.cFactor38, Form1.cFactor39, Form1.cFactor40,
                           Form1.cFactor41, Form1.cFactor42, Form1.cFactor43, Form1.cFactor44, Form1.cFactor45, Form1.cFactor46, Form1.cFactor47, Form1.cFactor48,
                           Form1.cFactor49, Form1.cFactor50, Form1.cFactor51, Form1.cFactor52}


    Public Shared ReadOnly n As TextBox() = {Form1.nFactor1, Form1.nFactor2, Form1.nFactor3, Form1.nFactor4, Form1.nFactor5, Form1.nFactor6, Form1.nFactor7, Form1.nFactor8,
                          Form1.nFactor9, Form1.nFactor10, Form1.nFactor11, Form1.nFactor12, Form1.nFactor13, Form1.nFactor14, Form1.nFactor15, Form1.nFactor16,
                          Form1.nFactor17, Form1.nFactor18, Form1.nFactor19, Form1.nFactor20, Form1.nFactor21, Form1.nFactor22, Form1.nFactor23, Form1.nFactor24,
                          Form1.nFactor25, Form1.nFactor26, Form1.nFactor27, Form1.nFactor28, Form1.nFactor29, Form1.nFactor30, Form1.nFactor31, Form1.nFactor32,
                          Form1.nFactor33, Form1.nFactor34, Form1.nFactor35, Form1.nFactor36, Form1.nFactor37, Form1.nFactor38, Form1.nFactor39, Form1.nFactor40,
                          Form1.nFactor41, Form1.nFactor42, Form1.nFactor43, Form1.nFactor44, Form1.nFactor45, Form1.nFactor46, Form1.nFactor47, Form1.nFactor48,
                          Form1.nFactor49, Form1.nFactor50, Form1.nFactor51, Form1.nFactor52}

    Public Shared ReadOnly cn As TextBox() = {Form1.cn1, Form1.cn2, Form1.cn3, Form1.cn4, Form1.cn5, Form1.cn6, Form1.cn7, Form1.cn8, Form1.cn9, Form1.cn10, Form1.cn11, Form1.cn12, Form1.cn13, Form1.cn14, Form1.cn15, Form1.cn16,
                           Form1.cn17, Form1.cn18, Form1.cn19, Form1.cn20, Form1.cn21, Form1.cn22, Form1.cn23, Form1.cn24,
                           Form1.cn25, Form1.cn26, Form1.cn27, Form1.cn28, Form1.cn29, Form1.cn30, Form1.cn31, Form1.cn32,
                           Form1.cn33, Form1.cn34, Form1.cn35, Form1.cn36, Form1.cn37, Form1.cn38, Form1.cn39, Form1.cn40,
                           Form1.cn41, Form1.cn42, Form1.cn43, Form1.cn44, Form1.cn45, Form1.cn46, Form1.cn47, Form1.cn48,
                           Form1.cn49, Form1.cn50, Form1.cn51, Form1.cn52}




    Private Shared x(100) As Integer

    Public Shared Property CNlevel As Integer()
        Get
            Return x
        End Get

        Set(value As Integer())
            x = value
        End Set

    End Property

    Private Shared y(100) As Integer

    Public Shared Property CNdirection As Integer()
        Get
            Return y
        End Get

        Set(value As Integer())
            y = value
        End Set

    End Property



End Class
