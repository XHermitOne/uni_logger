#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Тестирование UniLogger.
"""

import unittest
import os
import os.path
import sqlalchemy
import time


LOG_FILENAME = os.path.join(os.getcwd(), 'uni_logger.log')

# Параметры подключения к БД
DB_HOST = '10.0.0.30'
DB_PORT = 5432
DB_NAME = 'testing'
DB_USERNAME = 'xhermit'
DB_PASSWORD = 'xhermit'

DB_URL = 'postgres://%s:%s@%s:%d/%s' % (DB_USERNAME, DB_PASSWORD,
                                        DB_HOST, DB_PORT, DB_NAME)


class UniLoggerTest(unittest.TestCase):
    """
    Класс тестирования UniLogger.
    """
    srv = None

    @classmethod
    def setUpClass(cls):
        """
        Метод выполняется перед выполнением всех тестов.
        Метод действует на уровне класса, т.е. выполняется
        перед запуском тестов класса. 
        При этом синтаксис требует наличие декоратора @classmethod.
        """
        # Удаляем лог службы
        if os.path.exists(LOG_FILENAME):
            os.remove(LOG_FILENAME)                
    
        # Производим инсталляцию службы
        cmd = 'uni_logger.exe --tick=1000 --install'
        print(u'Запуск комманды: %s' % cmd)
        os.system(cmd)

        # Запуск службы
        cmd = 'net start UniLoggerService'
        print(u'Запуск комманды: %s' % cmd)
        os.system(cmd)

    @classmethod
    def tearDownClass(cls):
        """
        Метод выполняется после выполнения всех тестов.
        Запускается после выполнения всех тестов класса,
        требует наличия декоратора @classmethod.
        """
        # Останов службы
        cmd = 'net stop UniLoggerService'
        print(u'Запуск комманды: %s' % cmd)
        os.system(cmd)

        # Производим деинсталляцию службы
        cmd = 'uni_logger.exe --uninstall'
        print(u'Запуск комманды: %s' % cmd)
        os.system(cmd)  

    def setUp(self):
        """
        Метод вызывается перед запуском теста.
        Как правило, используется для подготовки окружения  для теста.
        """
        # Проверяем связь с БД
        self.db_connection = sqlalchemy.create_engine(DB_URL, echo=False)
        self.assertIsNotNone(self.db_connection)

    def tearDown(self):
        """
        Метод вызывается после завершения работы теста.
        Используется для 'приборки' за тестом.
        """
        self.db_connection.dispose()
        self.db_connection = None

    # ВНИМАНИЕ! 
    # Индекс в имени метода теста ставим для определения порядка выполнения тестов
    #          v 
    def test_0001_run(self):
        """
        Простой тест проверки связи со службой.
        """
        time.sleep(20)


if __name__ == '__main__':
    unittest.main()

