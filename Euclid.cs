using System;
using System.Collections.Generic;

// 点クラス（集合論的基盤）
public class Point
{
    public double X { get; }
    public double Y { get; }

    public Point(double x, double y)
    {
        X = x;
        Y = y;
    }

    // 距離関数（体論）
    public double DistanceTo(Point other)
    {
        return Math.Sqrt(Math.Pow(X - other.X, 2) + Math.Pow(Y - other.Y, 2));
    }

    public override string ToString() => $"({X}, {Y})";
}

// 線クラス（集合論：点の集合）
public class Line
{
    public Point P1 { get; }
    public Point P2 { get; }

    public Line(Point p1, Point p2)
    {
        if (p1.X == p2.X && p1.Y == p2.Y)
            throw new ArgumentException("Points must be distinct.");
        P1 = p1;
        P2 = p2;
    }

    // 平行性の判定（群論的変換を簡略化）
    public bool IsParallelTo(Line other)
    {
        double slope1 = (P2.Y - P1.Y) / (P2.X - P1.X + 1e-10); // ゼロ除算回避
        double slope2 = (other.P2.Y - other.P1.Y) / (other.P2.X - other.P1.X + 1e-10);
        return Math.Abs(slope1 - slope2) < 1e-10;
    }
}

// ユークリッド変換（群論：平行移動）
public class EuclideanTransformation
{
    public static Point Translate(Point p, double dx, double dy)
    {
        return new Point(p.X + dx, p.Y + dy);
    }

    // 回転（直交群 O(2) の簡略化）
    public static Point Rotate(Point p, double angleRad)
    {
        double cos = Math.Cos(angleRad);
        double sin = Math.Sin(angleRad);
        double x = p.X * cos - p.Y * sin;
        double y = p.X * sin + p.Y * cos;
        return new Point(x, y);
    }
}

// 圏論的構造の簡略化（ユークリッド空間の圏）
public class EuclideanCategory
{
    private List<Point> Points { get; } = new List<Point>();
    private List<Line> Lines { get; } = new List<Line>();

    public void AddPoint(Point p) => Points.Add(p);
    public void AddLine(Line l) => Lines.Add(l);

    // 射の例：点の変換
    public Point ApplyTransformation(Point p, Func<Point, Point> transform)
    {
        return transform(p);
    }
}

class Program
{
    static void Main()
    {
        // 点と線の生成
        Point p1 = new Point(0, 0);
        Point p2 = new Point(1, 1);
        Point p3 = new Point(0, 2);
        Point p4 = new Point(1, 3);

        Line l1 = new Line(p1, p2);
        Line l2 = new Line(p3, p4);

        // 距離の計算
        Console.WriteLine($"Distance between {p1} and {p2}: {p1.DistanceTo(p2)}");

        // 平行性の判定
        Console.WriteLine($"Are lines parallel? {l1.IsParallelTo(l2)}");

        // 変換の適用
        Point translated = EuclideanTransformation.Translate(p1, 2, 3);
        Console.WriteLine($"Translated point: {translated}");

        Point rotated = EuclideanTransformation.Rotate(p1, Math.PI / 2);
        Console.WriteLine($"Rotated point: {rotated}");

        // 圏論的構造のテスト
        EuclideanCategory category = new EuclideanCategory();
        category.AddPoint(p1);
        category.AddLine(l1);
        Point transformed = category.ApplyTransformation(p1, p => EuclideanTransformation.Rotate(p, Math.PI));
        Console.WriteLine($"Transformed point in category: {transformed}");
    }
}
