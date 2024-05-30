Public Class Henry


    Public Shared Function UnitlessVolumetric(ByVal vaporPressure As Single, ByVal solubility As Single, ByVal molecularWt As Single) As Single
        'vaporPressure  in torr
        'solubility in mg/L
        'molecularWt in g/mol

        UnitlessVolumetric = vaporPressure / 760.0 / (solubility / molecularWt) / (0.00008206) / 298.15  '(mass/vol) /(mass/vol) at 25C
    End Function


    Public Shared Function AtmM3Mol(ByVal vaporPressure As Single, ByVal solubility As Single, ByVal molecularWt As Single) As Single
        'vaporPressure  in torr
        'solubility in mg/L
        'molecularWt in g/mol

        AtmM3Mol = vaporPressure / 760.0 / (solubility / molecularWt)
    End Function



End Class
