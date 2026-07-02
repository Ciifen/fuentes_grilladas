# =============================================================================
# fuentes_grilladas — Descarga de datos climáticos e hidrológicos en formato ráster
# -----------------------------------------------------------------------------
# Copyright (C) 2025 CIIFEN — Centro Internacional para la Investigación
# del Fenómeno de El Niño (https://ciifen.org)
# Author       : Iliana Salazar
#                Desarrolladora de Servicios Climáticos
#                CIIFEN
# Project      : Proyecto ENANDES
# System       : Plataforma web de productos y servicios climáticos
# Repository   : https://github.com/Ciifen/fuentes_grilladas
# Contact      : Pier Maquilón — p.maquilon@ciifen.org
#
# All products resulting from this work are the exclusive property of CIIFEN.
# Attribution to the author must be preserved in all copies and
# derivative works.
# =============================================================================

#!/opt/shiny-server/samples/sample-apps/datosgrillados/ERA5/api_cds/my_env/bin/python3
import sys
import cdsapi
import threading
from datetime import datetime, timedelta

parametro1 = sys.argv[1]
parametro2 = sys.argv[2]
parametro3 = sys.argv[3]

nombre_archivo = parametro1
fecha_inicial = parametro2
fecha_final = parametro3

def obtener_fechas_entre(fecha_inicial_str, fecha_final_str):
    """
    Obtiene la lista de años, meses y días entre dos fechas en formato string 'yyyy-mm-dd'.

    Args:
        fecha_inicial_str: Fecha de inicio en formato string 'yyyy-mm-dd'.
        fecha_final_str: Fecha de fin en formato string 'yyyy-mm-dd'.

    Returns:
        Una tupla con tres listas: años, meses y días entre las fechas.
    """
    fecha_inicial = datetime.strptime(fecha_inicial_str, '%Y-%m-%d')
    fecha_final = datetime.strptime(fecha_final_str, '%Y-%m-%d')

    anios_str = []
    meses_str = []
    dias_str = []

    fecha_actual = fecha_inicial
    while fecha_actual <= fecha_final:
        # Convertir a string antes de agregar a la lista
        anio_actual_str = str(fecha_actual.year)
        mes_actual_str = fecha_actual.strftime('%m')
        dia_actual_str = fecha_actual.strftime('%d')

        if anio_actual_str not in anios_str:
            anios_str.append(anio_actual_str)
        if mes_actual_str not in meses_str:
            meses_str.append(mes_actual_str)
        if dia_actual_str not in dias_str:
            dias_str.append(dia_actual_str)

        fecha_actual += timedelta(days=1)

    return anios_str, meses_str, dias_str


def generarDataset(nombre_archivo,fecha_inicial,fecha_final):
    years, months, days = obtener_fechas_entre(fecha_inicial, fecha_final)
    database = 'reanalysis-era5-single-levels'
    c = cdsapi.Client()
    c.retrieve(
        database,
        {
            'variable': ['100m_u_component_of_wind', '100m_v_component_of_wind', 'maximum_2m_temperature_since_previous_post_processing',
            'minimum_2m_temperature_since_previous_post_processing', 'total_precipitation', 'volumetric_soil_water_layer_1',
            'volumetric_soil_water_layer_2', 'volumetric_soil_water_layer_3',],
            'product_type': ['reanalysis'],
            'year': years,
            'month': months,
            'day': days,
            'time': [
                '00:00', '01:00', '02:00',
                '03:00', '04:00', '05:00',
                 '06:00', '07:00', '08:00',
                '09:00', '10:00', '11:00',
                '12:00', '13:00', '14:00',
                '15:00', '16:00', '17:00',
                '18:00', '19:00', '20:00',
                '21:00', '22:00', '23:00',
            ],
            'area': [13, -110, -60, -58,],
            'data_format': 'netcdf',
            'download_format': 'unarchived',
             
        },
        nombre_archivo
    )

#generarDataset(nombre_archivo,year,month,day)
x = threading.Thread(target=generarDataset, args=(nombre_archivo, fecha_inicial, fecha_final))
x.start()
