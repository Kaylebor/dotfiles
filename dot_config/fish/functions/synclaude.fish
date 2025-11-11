function synclaude
    ANTHROPIC_BASE_URL=https://api.synthetic.new/anthropic \
        ANTHROPIC_AUTH_TOKEN=$SYNTHETIC_API_KEY \
        ANTHROPIC_DEFAULT_OPUS_MODEL=hf:zai-org/GLM-4.6 \
        ANTHROPIC_DEFAULT_SONNET_MODEL=hf:openai/gpt-oss-120b \
        ANTHROPIC_DEFAULT_HAIKU_MODEL=hf:meta-llama/Llama-4-Scout-17B-16E-Instruct \
        CLAUDE_CODE_SUBAGENT_MODEL=hf:moonshotai/Kimi-K2-Thinking \
        CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC=1 \
        claude $argv
end
