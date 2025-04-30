using System;
using System.Collections.Generic;

// クラウドAPIセキュリティモデル: 認証と認可のシミュレーション
// 多層: クライアント-サーバー間通信を模擬
// 多要素: APIキー認証、認可チェック、データモデル
// 多次元: リソースごとのアクセス権限管理
namespace CloudApiSecurity
{
    // リソースクラス
    public class Resource
    {
        public string Id { get; set; }
        public string Owner { get; set; }
        public string Status { get; set; }
    }

    // APIサーバークラス
    public class CloudApiServer
    {
        private List<Resource> resources = new List<Resource>();
        private const string ValidApiKey = "valid_key";

        // 認証メソッド
        private bool Authenticate(string apiKey)
        {
            if (apiKey == ValidApiKey)
            {
                Console.WriteLine("Authentication successful");
                return true;
            }
            Console.WriteLine("Authentication failed");
            return false;
        }

        // 認可メソッド
        private bool Authorize(string apiKey, string owner)
        {
            // 簡略化: APIキーが正しければ特定の所有者を認可
            if (apiKey == ValidApiKey && owner == "user1")
            {
                Console.WriteLine($"Authorization successful for {owner}");
                return true;
            }
            Console.WriteLine($"Authorization failed for {owner}");
            return false;
        }

        // リソース作成メソッド
        public bool CreateResource(string apiKey, string id, string owner)
        {
            if (!Authenticate(apiKey))
                return false;

            if (!Authorize(apiKey, owner))
                return false;

            if (resources.Count >= 10)
            {
                Console.WriteLine("Resource limit reached");
                return false;
            }

            resources.Add(new Resource { Id = id, Owner = owner, Status = "RUNNING" });
            Console.WriteLine($"Resource created: {id} by {owner}");
            return true;
        }

        // リソース取得メソッド
        public bool GetResource(string apiKey, string id, string owner, out string status)
        {
            status = null;
            if (!Authenticate(apiKey))
                return false;

            if (!Authorize(apiKey, owner))
                return false;

            var resource = resources.Find(r => r.Id == id && r.Owner == owner);
            if (resource != null)
            {
                status = resource.Status;
                Console.WriteLine($"Resource found: {id} Status: {status}");
                return true;
            }

            Console.WriteLine($"Resource not found or unauthorized: {id}");
            return false;
        }
    }

    // クライアント側テスト
    class Program
    {
        static void Main(string[] args)
        {
            var server = new CloudApiServer();

            // リソース作成テスト
            server.CreateResource("valid_key", "vm1", "user1");
            server.CreateResource("invalid_key", "vm2", "user1");
            server.CreateResource("valid_key", "vm3", "user2");

            // リソース取得テスト
            string status;
            server.GetResource("valid_key", "vm1", "user1", out status);
            server.GetResource("valid_key", "vm1", "user2", out status);
        }
    }
}
