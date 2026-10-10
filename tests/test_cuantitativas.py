import unittest
import csv
import math

class TestQuantitativeAnalysis(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        with open("insurance.csv", "r", encoding="utf-8") as f:
            reader = csv.DictReader(f)
            cls.rows = list(reader)
        cls.n = len(cls.rows)
        assert cls.n == 1338

    def _stats(self, col):
        vals = sorted([float(r[col]) for r in self.rows])
        n = len(vals)
        mean = sum(vals) / n
        var = sum((x - mean)**2 for x in vals) / (n - 1)
        sd = math.sqrt(var)
        cv = (sd / mean) * 100
        median = (vals[n // 2 - 1] + vals[n // 2]) / 2.0 if n % 2 == 0 else vals[n // 2]
        
        def rq(p):
            idx = 1 + (n - 1) * p
            lo = int(math.floor(idx))
            hi = int(math.ceil(idx))
            h = idx - lo
            return (1 - h) * vals[lo - 1] + h * vals[hi - 1] if hi <= n else vals[-1]

        q1 = rq(0.25)
        q3 = rq(0.75)
        iqr = q3 - q1
        outliers = [x for x in vals if x < q1 - 1.5 * iqr or x > q3 + 1.5 * iqr]
        return {
            "mean": mean, "median": median, "sd": sd, "cv": cv,
            "min": vals[0], "max": vals[-1], "q1": q1, "q3": q3, "iqr": iqr,
            "outliers_count": len(outliers)
        }

    def test_age(self):
        s = self._stats("age")
        self.assertAlmostEqual(s["mean"], 39.21, places=2)
        self.assertEqual(s["median"], 39.0)
        self.assertAlmostEqual(s["sd"], 14.05, places=2)
        self.assertAlmostEqual(s["cv"], 35.84, places=1)
        self.assertEqual(s["outliers_count"], 0)

    def test_children(self):
        s = self._stats("children")
        self.assertAlmostEqual(s["mean"], 1.09, places=2)
        self.assertEqual(s["median"], 1.0)
        self.assertAlmostEqual(s["sd"], 1.21, places=2)
        self.assertAlmostEqual(s["cv"], 110.10, places=1)
        self.assertEqual(s["outliers_count"], 0)

    def test_bmi(self):
        s = self._stats("bmi")
        self.assertAlmostEqual(s["mean"], 30.66, places=2)
        self.assertAlmostEqual(s["median"], 30.40, places=2)
        self.assertAlmostEqual(s["sd"], 6.10, places=2)
        self.assertAlmostEqual(s["cv"], 19.89, places=1)
        self.assertEqual(s["outliers_count"], 9)

    def test_charges(self):
        s = self._stats("charges")
        self.assertAlmostEqual(s["mean"], 13270.42, places=1)
        self.assertAlmostEqual(s["median"], 9382.03, places=1)
        self.assertAlmostEqual(s["sd"], 12110.01, places=1)
        self.assertAlmostEqual(s["cv"], 91.26, places=1)
        self.assertEqual(s["outliers_count"], 139)

if __name__ == "__main__":
    unittest.main()
